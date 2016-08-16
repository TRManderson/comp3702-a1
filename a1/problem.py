from typing import Dict, List, Set
import a1.util as u
import heapq as hq
from collections import namedtuple


class Problem(object):
    def __init__(self, graph: Dict[int, Dict[int, float]], debug: bool = False) -> None:
        self.graph = graph
        self.debug = debug

        # used in A*
        self.heuristic_cache = {}  # type Dict[(int, int), float]
        self.cost_incident = {}  # type: Dict[int, float]

        # fill self.cost_incident
        for node, children in self.graph.items():
            for child, cost in children.items():
                if child not in self.cost_incident or cost < self.cost_incident[child]:
                    self.cost_incident[child] = cost

    def query(self, query: u.Query) -> List[int]:
        if self.debug:
            print("Performing query: {}".format(str(query)))
        result = getattr(self, query.alg.name)(
            query.initial, query.goal
        )  # type: List[int]
        if self.debug:
            print()
        return result

    def astar_heuristic(self, current: int, goal: int) -> float:
        if (current, goal) not in self.heuristic_cache:
            if self.graph[current]:
                # If there are outgoing edges, take the smallest cost to
                # traverse one
                result = min(self.graph[current].values())
            else:
                # If there aren't outgoing edges, assume there is no cost
                result = 0
            if goal not in self.graph[current]:
                # If the goal is not an adjacent node, we have to a traverse an
                # edge that is incident on it
                result += self.cost_incident[goal]
            self.heuristic_cache[(current, goal)] = result
        return self.heuristic_cache[(current, goal)]

    def astar(self, initial: int, goal: int) -> (int, List[int]):
        heap = [(self.astar_heuristic(initial, goal), 0, initial, [])]

        visited = set()  # type: Set[int]

        while heap != []:
            c_estimated, c_so_far, current, path = hq.heappop(heap)

            if self.debug:
                print(c_estimated, c_so_far, current, path)

            if current in visited:
                continue
            visited.add(current)

            new_path = path + [current]

            if current == goal:
                return c_so_far, new_path

            for next_node, step_cost in self.graph[current].items():
                new_cost = c_so_far + step_cost
                hq.heappush(heap, (
                    self.astar_heuristic(next_node, goal) + new_cost,
                    new_cost,
                    next_node,
                    new_path,
                ))

        return 0, []

    def uniform(self, initial: int, goal: int) -> (int, List[int]):
        # We start the heap with a cost of 0 at the initial node, with no path taken yet
        heap = [(0, initial, [])]

        best_to = {}

        while heap != []:
            cost, current, path = hq.heappop(heap)
            if self.debug:
                print(cost, current, path)

            # If we've seen a better path to this node, don't check from here
            if current in best_to and best_to[current] < cost:
                continue

            newpath = path + [current]

            # If we're at the goal node, we're done
            if current == goal:
                if self.debug:
                    print("Reached goal.")
                return cost, newpath

            # Add all the child nodes
            for next_node, step_cost in self.graph[current].items():
                hq.heappush(heap, (cost + step_cost, next_node, newpath))

            best_to[current] = cost
        return 0, []
