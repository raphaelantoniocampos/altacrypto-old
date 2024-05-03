from models.user_settings import UserSettings


class GlobalSettings:
    """TODO: Document Class"""

    INTERVALS_IN_MINUTES = [5, 10, 15, 30, 60]
    EXECUTION_FREQUENCY_MINUTES = 1
    STANDARD_USER_SETTINGS = UserSettings()
