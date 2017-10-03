package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.Deque;
import java.util.concurrent.LinkedBlockingDeque;
import java.util.NoSuchElementException;
import java.util.concurrent.CancellationException;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver extends SequentialSolver {
    /**
     * The nodes in the maze to be visited next. Using a stack
     * implements a search that goes depth first..
     */
    protected Deque<Integer> frontier;

    private int steps;
    private int player;

    private boolean stop;

    private ForkJoinSolver parent;
    private ForkJoinSolver solver1;
    private ForkJoinSolver solver2;

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze   the maze to be searched
     */
    public ForkJoinSolver(Maze maze) {
        super(maze);

        this.start = maze.start();

        steps = 0;
        player = -1;
        stop = false;
    }

    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze        the maze to be searched
     * @param forkAfter   the number of steps (visited nodes) after
     *                    which a parallel task is forked; if
     *                    <code>forkAfter &lt;= 0</code> the solver never
     *                    forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter) {
        this(maze);

        this.forkAfter = forkAfter;
        initStructures();
    }

    public ForkJoinSolver(Maze maze, int forkAfter, Set<Integer> visited,
            Map<Integer, Integer> predecessor, Deque<Integer> frontier,
            ForkJoinSolver parent) {
        this(maze, forkAfter);

        this.parent = parent;
        this.visited = visited;
        this.predecessor = predecessor;
        this.frontier = frontier;
    }

    public ForkJoinSolver(Maze maze, int forkAfter, Set<Integer> visited,
            Map<Integer, Integer> predecessor, Deque<Integer> frontier,
            ForkJoinSolver parent, int player) {
        this(maze, forkAfter, visited, predecessor, frontier, parent);
        this.player = player;
    }

    @Override
    protected void initStructures() {
        this.visited = new ConcurrentSkipListSet<>();
        this.predecessor = new ConcurrentHashMap<>();
        this.frontier = new LinkedBlockingDeque<>();
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return   the list of node identifiers from the start node to a
     *           goal node in the maze; <code>null</code> if such a path cannot
     *           be found.
     */
    @Override
    public List<Integer> compute() {
        return parallelDepthFirstSearch();
    }

    private List<Integer> parallelDepthFirstSearch() {
        
        if (player == -1) player = maze.newPlayer(start);
        if (!visited.contains(start)) frontier.push(start);

        while (!frontier.isEmpty() && !stop) {

            if (steps < forkAfter || frontier.size() <= 1) {

                List<Integer> result = sequentialStep();
                if (result != null) return result;

            } else {
                return forkOperations();
            }
        }
        return null;
    }

    private List<Integer> sequentialStep() {
        int current;
        try {
            current = frontier.pop();

            // This happens when another thread has emptied frontier
        } catch (NoSuchElementException e) {
            return null;
        }

        if (maze.hasGoal(current)) {
            maze.move(player, current);
            parent.stop();
            return pathFromTo(start, current);
        }

        if (!visited.contains(current)) {
            System.out.println("Visited: "+current+", player: "+player);
            visited.add(current);
            maze.move(player, current);
            steps++;

            for (int nb : maze.neighbors(current)) {
                frontier.push(nb);
                if (!visited.contains(nb))
                    predecessor.put(nb, current);
            }
        }

        return null;
    }

    private List<Integer> forkOperations() {
        solver1 = new ForkJoinSolver(maze, forkAfter,
            visited, predecessor, frontier, this);
        solver1.fork();

        solver2 = new ForkJoinSolver(maze, forkAfter,
            visited, predecessor, frontier, this, player);

        List<Integer> solution2 = solver2.compute();
        if (solution2 != null) return solution2;

        List<Integer> solution1 = solver1.join();
        return solution1;
    }

    public void stop() {
        if (!stop) {
            stop = true;

            System.out.println("STOP player: "+player);

            try { solver1.stop(); } catch (Exception e) { }
            try { solver2.stop(); } catch (Exception e) { }

            if (parent != null) parent.stop();
        }
    }
}