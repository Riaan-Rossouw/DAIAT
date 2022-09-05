@echo off
    setlocal ENABLEDELAYEDEXPANSION

    for %%F in (type-C_*.csv) do (
        set name="%%F"
        ren "%%F" "!name:~17,6!.csv"
    )