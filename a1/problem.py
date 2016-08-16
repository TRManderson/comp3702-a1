from typing import Dict, List
import a1.util as u
import heapq as hq
from collections import namedtuple


class Problem(object):
    def __init__(self, graph: Dict[int, Dict[int, float]], debug: bool = False) -> None:
        self.graph = graph
        self.debug = debug

    def query(self, query: u.Query) -> List[int]:
        if self.debug:
            print("Performing query: {}".format(str(query)))
        result = getattr(self, query.alg.name)(query.initial, query.goal) # type: List[int]
        if self.debug:
            print()
        return result

    def astar(self, initial: int, goal: int) -> (int, List[int]):
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
            if current in best_to and best_to[current].cost < cost:
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
