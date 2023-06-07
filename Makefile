PROJECT = pockettram2
PROJECT_DESCRIPTION = Pocket tram v2
PROJECT_VERSION = 0.1.0

DEPS = cowboy jiffy oauth2_client hackney jsx
dep_cowboy_commit = 2.10.0
dep_oauth2_client = git https://github.com/kivra/oauth2_client v1.4.2
dep_hackney_commit = 1.18.0
dep_jsx_commit = v2.11.0

DEP_PLUGINS = cowboy

BUILD_DEPS += relx
include erlang.mk
