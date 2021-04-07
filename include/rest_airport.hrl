

-define(RECORD_TYPE,      airport).
-define(RECORD_KEY_FIELD, code).

-record(?RECORD_TYPE,
        {?RECORD_KEY_FIELD, city, country, name }).

