from managers.data_manager import DataManager
from utils.user_settings import UserSettings

def refresh_database() -> None:
    user_settings = UserSettings()
    data_manager = DataManager(user_settings.db_path, user_settings)
    data_manager.drop_table("Assets")

