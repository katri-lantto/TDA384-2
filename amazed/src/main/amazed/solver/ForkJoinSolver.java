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
import java.util.concurrent.atomic.AtomicBoolean;

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
    private AtomicBoolean stop;

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
        stop = new AtomicBoolean();
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
            int start, AtomicBoolean stop) {
        this(maze, forkAfter);
        
        this.visited = visited;
        this.start = start;
        this.frontier.push(start);
        this.stop = stop;
    }

    private ForkJoinSolver(Maze maze, int forkAfter, Set<Integer> visited,
            int start, AtomicBoolean stop, int player) {
        this(maze, forkAfter, visited, start, stop);

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

        while (!frontier.isEmpty() && !this.stop.get()) {

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
            this.stop.set(true);

            List<Integer> path = pathFromTo(this.start, current);
            return path;
        }

        if (!visited.contains(current)) {
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
        List<ForkJoinSolver> childList = new ArrayList<>();

        int firstUnvisited = 0;
        do {
            if (this.frontier.empty() || this.stop.get()) return null;

            firstUnvisited = this.frontier.pop();
        } while (visited.contains(firstUnvisited));

        while(!this.frontier.isEmpty()) {
            if (this.stop.get()) return null;

            int node = this.frontier.pop();
            if (!this.visited.contains(node)) {

                ForkJoinSolver secondaryChild = new ForkJoinSolver(maze,
                    forkAfter, visited, node, this.stop);
                secondaryChild.fork();
                childList.add(secondaryChild);
            }
        }

        ForkJoinSolver primaryChild = new ForkJoinSolver(maze, forkAfter,
            visited, firstUnvisited, this.stop, player);

        List<Integer> primarySolution = primaryChild.compute();
        if (primarySolution != null)
            return addPath(primarySolution, primaryChild.start);

        for (ForkJoinSolver secondaryChild : childList) {
            List<Integer> secondarySolution = secondaryChild.join();
            if (secondarySolution != null)
                return addPath(secondarySolution, secondaryChild.start);
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
}
