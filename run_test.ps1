gcc test_z80.c z80.c -o z80test.exe
if ( $LastExitCode -eq 0)
{
    ./z80test.exe
}
