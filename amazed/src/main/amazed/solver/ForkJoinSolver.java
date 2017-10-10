package amazed.solver;

import amazed.maze.Maze;

import java.lang.NullPointerException;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
// import java.util.Deque;
// import java.util.ArrayDeque;
import java.util.Stack;
// import java.util.NoSuchElementException;
// import java.util.EmptyStackException;
// import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentSkipListSet;
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
    // protected Deque<Integer> frontier;

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
        // initStructures();
    }

    private ForkJoinSolver(Maze maze, int forkAfter, Set<Integer> visited,
            ForkJoinSolver parent) {

        this(maze, forkAfter);
        this.parent = parent;
        this.visited = visited;
        // this.predecessor = predecessor;
    }


    private ForkJoinSolver(Maze maze, int forkAfter, Set<Integer> visited,
            ForkJoinSolver parent, int start) {
        this(maze, forkAfter, visited, parent);
        this.start = start;
        this.frontier.push(start);
    }

    private ForkJoinSolver(Maze maze, int forkAfter, Set<Integer> visited,
            ForkJoinSolver parent, int start, Map<Integer, Integer> predecessor,
            Stack<Integer> frontier, int player) {
        this(maze, forkAfter, visited, parent);
        this.start = start;
        this.frontier = frontier;
        this.predecessor = predecessor;
        this.player = player;
    }

    @Override
    protected void initStructures() {
        this.visited = new ConcurrentSkipListSet<>();
        // this.predecessor = new ConcurrentHashMap<>();
        this.predecessor = new HashMap<>();
        // this.frontier = new ArrayDeque<>();
        this.frontier = new Stack<>();
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

        if (!visited.contains(start)) {
          System.out.println("Pushar start");
          frontier.push(this.start);
        }


        if (this.player == -1) player = maze.newPlayer(this.start);

        while (!frontier.isEmpty() && !this.stop) {

            if (this.steps < this.forkAfter) {

                List<Integer> result = sequentialDepthFirstStep();
                if (result != null) return result;

            } else {

              if (frontier.size() == 1) {
                return singleForkOperation();

              } else {
                int first;
                // try {
                    do {
                        if (this.frontier.empty()) return null;

                        first = this.frontier.pop();
                    } while (visited.contains(first));

                //     // This only happens when another thread has emptied frontier
                // } catch (EmptyStackException e) {
                //     return null;
                // }
                // int first = this.frontier.pop();
                return forkOperations(first);
              }
            }
        }
        return null;
    }

    // Basically the same as SequentialSolver's depthFirstSearch(),
    // but without the loop
    private List<Integer> sequentialDepthFirstStep() {
        if (this.frontier.empty()) return null;

        int current = frontier.pop();

        // int current;
        // try {
            // current = frontier.pop();

        //     // This only happens when another thread has emptied frontier
        // } catch (EmptyStackException e) {
        //     return null;
        // }

        if (maze.hasGoal(current)) {
            maze.move(this.player, current);

            this.stop = true;
            if (this.parent != null) this.parent.stop();

            List<Integer> path = pathFromTo(this.start, current);
            // System.out.println("Goal! : "+path);
            return path;
            // return pathFromTo(this.start, current);
        }

        if (!visited.contains(current)) {
            System.out.println("Visited: "+current+", player: "+player);

            visited.add(current);
            maze.move(this.player, current);
            this.steps++;

            for (int nb : maze.neighbors(current)) {
                frontier.push(nb);
                if (!visited.contains(nb))
                    predecessor.put(nb, current);
            }
        }

        return null;
    }

    private List<Integer> singleForkOperation() {
        solver1 = new ForkJoinSolver(maze, forkAfter,
            visited, this, this.start, predecessor, frontier, player);
        // solver1.fork();
        // List<Integer> solution1 = solver1.join();
        List<Integer> solution1 = solver1.compute();
        return addPath(solution1, solver1.start);
    }

    private List<Integer> forkOperations(int first) {
        solver1 = new ForkJoinSolver(maze, forkAfter,
            visited, this, first);
        solver1.fork();

        solver2 = new ForkJoinSolver(maze, forkAfter,
            visited, this, this.start, predecessor, frontier, player);

        List<Integer> solution2 = solver2.compute();
        if (solution2 != null) return addPath(solution2, solver2.start);

        List<Integer> solution1 = solver1.join();
        return addPath(solution1, solver1.start);
    }

    private List<Integer> addPath(List<Integer> oldAnswer, int end) {
        if (oldAnswer != null) {
            // System.out.println("1. "+oldAnswer);
            oldAnswer.remove(0);
            List<Integer> newAnswer = pathFromTo(this.start, end);
            newAnswer.addAll(oldAnswer);
            // System.out.println("2. "+newAnswer+"\n----");
            return newAnswer;
        }
        
        return null;
    }

    // Stops other processes when the goal is found
    public void stop() {
        if (!stop) {
            stop = true;

            // System.out.println("STOP player: "+player);

            // Catching null pointer exceptions, instead of checking if null
            // because when doing this concurrently, a check may allready
            // be outdated when doing the stop() operation
            try { solver1.stop(); } catch (NullPointerException e) { }
            try { solver2.stop(); } catch (NullPointerException e) { }

            if (parent != null) parent.stop();
        }
    }
}
