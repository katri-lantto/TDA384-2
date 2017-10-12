package amazed.solver;

import amazed.maze.Maze;

import java.lang.NullPointerException;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.SortedSet;
import java.util.Stack;
import java.util.concurrent.ConcurrentSkipListSet;

/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */


public class ForkJoinSolver extends SequentialSolver {

    private int steps;
    private int player;

    private boolean stop;

    private ForkJoinSolver parent;

    private ForkJoinSolver solver;
    private List<ForkJoinSolver> solverList;

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
    }

    private ForkJoinSolver(Maze maze, int forkAfter, Set<Integer> visited,
            ForkJoinSolver parent) {
        this(maze, forkAfter);
        
        this.parent = parent;
        this.visited = visited;
    }


    private ForkJoinSolver(Maze maze, int forkAfter, Set<Integer> visited,
            ForkJoinSolver parent, int start) {
        this(maze, forkAfter, visited, parent);

        this.start = start;
        this.frontier.push(start);
    }

    private ForkJoinSolver(Maze maze, int forkAfter, Set<Integer> visited,
            ForkJoinSolver parent, int start, int player) {
        this(maze, forkAfter, visited, parent, start);

        this.player = player;
    }

    @Override
    protected void initStructures() {
        this.visited = new ConcurrentSkipListSet<>();
        this.predecessor = new HashMap<>();
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

        if (!visited.contains(start)) frontier.push(this.start);
        if (this.player == -1) player = maze.newPlayer(this.start);

        while (!frontier.isEmpty() && !this.stop) {

            if (this.steps < this.forkAfter) {
                List<Integer> result = sequentialDepthFirstStep();
                if (result != null) return result;

            } else {
                return forkOperation();
            }
        }

        return null;
    }

    // Basically the same as SequentialSolver's depthFirstSearch(),
    // but without the loop
    private List<Integer> sequentialDepthFirstStep() {
        if (this.frontier.empty()) return null;

        int current = frontier.pop();

        if (maze.hasGoal(current)) {
            maze.move(this.player, current);

            this.stop = true;
            if (this.parent != null) this.parent.stop();

            List<Integer> path = pathFromTo(this.start, current);
            return path;
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

    private List<Integer> forkOperation() {
        this.solverList = new ArrayList<>();

        int firstUnvisited = 0;
        do {
            if (this.frontier.empty() || this.stop) return null;

            firstUnvisited = this.frontier.pop();
        } while (visited.contains(firstUnvisited));

        while(!this.frontier.isEmpty()) {
            if (this.stop) return null;

            int node = this.frontier.pop();
            if (!this.visited.contains(node)) {

                ForkJoinSolver otherSolver = new ForkJoinSolver(maze, forkAfter,
                    visited, this, node);
                otherSolver.fork();
                this.solverList.add(otherSolver);
            }
        }

        solver = new ForkJoinSolver(maze, forkAfter,
            visited, this, firstUnvisited, player);

        List<Integer> solution = solver.compute();
        if (solution != null) return addPath(solution, solver.start);

        for (ForkJoinSolver otherSolver : this.solverList) {
            List<Integer> otherSolution = otherSolver.join();
            if (otherSolution != null) return addPath(otherSolution, otherSolver.start);
        }

        return null;
    }

    private List<Integer> addPath(List<Integer> oldAnswer, int end) {
        if (oldAnswer != null) {
            oldAnswer.remove(0);
            List<Integer> newAnswer = pathFromTo(this.start, end);
            newAnswer.addAll(oldAnswer);
            return newAnswer;
        }

        return null;
    }

    // Stops other processes when the goal is found
    public void stop() {
        if (!stop) {
            stop = true;

            // Catching null pointer exceptions, instead of checking if null
            // because when doing this concurrently, a check may allready
            // be outdated when doing the stop() operation
            try { solver.stop(); } catch (NullPointerException e) { }

            for (ForkJoinSolver otherSolver : this.solverList) {
                try { otherSolver.stop(); } catch (NullPointerException e) { }
            }

            if (parent != null) parent.stop();
        }
    }
}
