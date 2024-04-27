from typing import TypedDict
from bson.timestamp import Timestamp


class SnapshotDict(TypedDict):
    """TODO: Document class"""

    timestamp: Timestamp
    price: float
