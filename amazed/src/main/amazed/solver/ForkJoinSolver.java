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

    public ForkJoinSolver(Maze maze, int forkAfter, Set<Integer> visited) {
        this(maze, forkAfter);
        this.visited = visited;
    }        

    @Override
    protected void initStructures() {
        visited = new ConcurrentSkipListSet<>();
        predecessor = new ConcurrentHashMap<>();
        frontier = new LinkedBlockingDeque<>();
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
        
        int player = maze.newPlayer(start);
        frontier.push(start);
        while (!frontier.isEmpty()) {

            int current = frontier.pop();

            if (maze.hasGoal(current)) {
                maze.move(player, current);
                return pathFromTo(start, current);
            }

            if (!visited.contains(current)) {
                // if (steps < forkAfter) {
                    maze.move(player, current);
                    visited.add(current);
                    steps++;

                    for (int nb : maze.neighbors(current)) {
                        frontier.push(nb);
                        if (!visited.contains(nb))
                            predecessor.put(nb, current);
                    }

                // } else {

                // }
            }
        }



        return null;
    }
}
