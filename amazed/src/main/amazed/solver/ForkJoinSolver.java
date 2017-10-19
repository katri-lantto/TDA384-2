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
        this.forkAfter = 1;
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

    // The following two constructors deals with the forking objects;
    // the new ForkJoinSolver objects that is created after every forkAfter steps.

    // This one take in addition also the two global data structures: visitied
    // and stop; and also the local starting position of this local fork.
    private ForkJoinSolver(Maze maze, int forkAfter, Set<Integer> visited,
            int start, AtomicBoolean stop) {
        this(maze, forkAfter);

        this.visited = visited;
        this.start = start;
        this.frontier.push(start);
        this.stop = stop;
    }

    // In addition to the above constructor, this one also re-use a player,
    // instead of creating a new one.
    private ForkJoinSolver(Maze maze, int forkAfter, Set<Integer> visited,
            int start, AtomicBoolean stop, int player) {
        this(maze, forkAfter, visited, start, stop);

        this.player = player;
    }

    // Since we need to share the visited data structure between forks, this
    // method is overridden to incorporate a concurrently safe data structure.
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

    // A big wrapper method for the whole algorithm, which loops until either
    // no more nodes is left to explore, or a goal is found.
    private List<Integer> parallelDepthFirstSearch() {

        if (!visited.contains(start)) frontier.push(this.start);
        if (this.player == -1) player = maze.newPlayer(this.start);

        while (!frontier.isEmpty() && !this.stop.get()) {

            // The sequential bit
            if (this.steps < this.forkAfter) {
                List<Integer> result = sequentialDepthFirstStep();
                if (result != null) return result;

            // The parallel bit
            } else {
                return forkOperation();
            }
        }

        return null;
    }

    // Basically the same as SequentialSolver's depthFirstSearch(),
    // but without the loop. Some small changes to make way for concurrent
    // forking.
    private List<Integer> sequentialDepthFirstStep() {
        if (this.frontier.empty()) return null;

        int current = frontier.pop();

        // When the goal is found, end search and return answer
        if (maze.hasGoal(current)) {
            maze.move(this.player, current);
            this.stop.set(true);

            return pathFromTo(this.start, current);
        }

        // Otherwise, search for new nodes to visit, and keep on going
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

    // Method for performing the fork operation, invoked by
    // the method parallelDepthFirstSearch() when it's time
    // to do a fork.
    private List<Integer> forkOperation() {
        List<ForkJoinSolver> childList = new ArrayList<>();

        // First we save the first unvisited node, for laters
        int firstUnvisited = 0;
        do {
            if (this.frontier.empty() || this.stop.get()) return null;

            firstUnvisited = this.frontier.pop();
        } while (visited.contains(firstUnvisited));

        // Then we create new fork objects for every other unvisted node
        while(!this.frontier.isEmpty()) {
            if (this.stop.get()) return null;

            // Pop a node from the stack of frontiers, check if this node is
            // already visited. If it is not, create a new instance of the
            // ForkJoinSolver class and fork. Also adding the new instance
            // to a list of children because we need to join them all in the end.
            int node = this.frontier.pop();
            if (!this.visited.contains(node)) {

                ForkJoinSolver secondaryChild = new ForkJoinSolver(maze,
                    forkAfter, visited, node, this.stop);
                secondaryChild.fork();
                childList.add(secondaryChild);
            }
        }

        // Now, the local child object is created, starting at the previously
        // saved first item in frontier, and is computed directly
        ForkJoinSolver primaryChild = new ForkJoinSolver(maze, forkAfter,
            visited, firstUnvisited, this.stop, player);

        List<Integer> primarySolution = primaryChild.compute();
        if (primarySolution != null)
            return addPath(primarySolution, primaryChild.start);

        // Lastly, if the solution is not allready found, we go through all the
        // children forks and see if any one of them has found the goal
        for (ForkJoinSolver secondaryChild : childList) {
            List<Integer> secondarySolution = secondaryChild.join();
            if (secondarySolution != null)
                return addPath(secondarySolution, secondaryChild.start);
        }

        return null;
    }

    // A utility method that takes a local solution from a child, and expand
    // it to incorporate this objects path. All of this is then returned,
    // so that its parent can keep on expanding the solution, until it is complete.
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
