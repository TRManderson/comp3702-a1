from typing import Dict, List
import a1.util as u
import heapq as hq
from collections import namedtuple


class Problem(object):
    def __init__(self, graph: Dict[int, Dict[int, float]]) -> None:
        self.graph = graph

    def query(self, query: u.Query) -> List[int]:
        result = getattr(self, query.alg.name)(query.initial, query.goal) # type: List[int]
        return result

    def astar(self, initial, goal):
        visited = set()


    def uniform(self, initial, goal):
        pass
