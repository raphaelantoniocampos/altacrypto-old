from managers.data_manager import DataManager


def refresh_database() -> None:
    data_manager = DataManager()
    data_manager.drop_table("Assets")