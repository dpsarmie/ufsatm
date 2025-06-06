program test_module_wrt_grid_comp
    use module_wrt_grid_comp, only: get_outfile, lambert, rtll, splat8, splat4
    implicit none
    
    ! Declare the module functions that we need to test
    interface
        pure function trim_regridmethod_suffix(string) result(trimmed_string)
            character(len=*), intent(in) :: string
            character(len=:), allocatable :: trimmed_string
        end function trim_regridmethod_suffix
        
        pure function trim_suffix(string, suffix) result(trimmed_string)
            character(len=*), intent(in) :: string, suffix
            character(len=:), allocatable :: trimmed_string
        end function trim_suffix
    end interface
    
    ! Test counters
    integer :: tests_passed = 0
    integer :: tests_failed = 0
    
    ! Run all tests
    call test_trim_regridmethod_suffix()
    call test_trim_suffix()
    call test_get_outfile()
    call test_lambert()
    call test_rtll()
    call test_splat8()
    call test_splat4()
    
    ! Print summary
    print *, "========================================="
    print *, "Test Summary:"
    print *, "Tests passed: ", tests_passed
    print *, "Tests failed: ", tests_failed
    print *, "========================================="
    
contains

    !---------------------------------------------------------------------------
    ! Test trim_regridmethod_suffix function
    !---------------------------------------------------------------------------
    subroutine test_trim_regridmethod_suffix()
        character(len=:), allocatable :: result
        character(len=100) :: test_name
        
        print *, "Testing trim_regridmethod_suffix..."
        
        ! Test 1: String with _bilinear suffix
        test_name = "trim_regridmethod_suffix with _bilinear"
        result = trim_regridmethod_suffix("output_grid_bilinear")
        call assert_string_equal(test_name, result, "output_grid")
        
        ! Test 2: String with _patch suffix
        test_name = "trim_regridmethod_suffix with _patch"
        result = trim_regridmethod_suffix("output_grid_patch")
        call assert_string_equal(test_name, result, "output_grid")
        
        ! Test 3: String with _nearest_stod suffix
        test_name = "trim_regridmethod_suffix with _nearest_stod"
        result = trim_regridmethod_suffix("output_grid_nearest_stod")
        call assert_string_equal(test_name, result, "output_grid")
        
        ! Test 4: String with _nearest_dtos suffix
        test_name = "trim_regridmethod_suffix with _nearest_dtos"
        result = trim_regridmethod_suffix("output_grid_nearest_dtos")
        call assert_string_equal(test_name, result, "output_grid")
        
        ! Test 5: String with _conserve suffix
        test_name = "trim_regridmethod_suffix with _conserve"
        result = trim_regridmethod_suffix("output_grid_conserve")
        call assert_string_equal(test_name, result, "output_grid")
        
        ! Test 6: String with no suffix
        test_name = "trim_regridmethod_suffix with no suffix"
        result = trim_regridmethod_suffix("output_grid")
        call assert_string_equal(test_name, result, "output_grid")
        
        ! Test 7: String with spaces
        test_name = "trim_regridmethod_suffix with spaces"
        result = trim_regridmethod_suffix("  output_grid_bilinear  ")
        call assert_string_equal(test_name, result, "output_grid")
        
        ! Test 8: Empty string
        test_name = "trim_regridmethod_suffix with empty string"
        result = trim_regridmethod_suffix("")
        call assert_string_equal(test_name, result, "")
        
    end subroutine test_trim_regridmethod_suffix
    
    !---------------------------------------------------------------------------
    ! Test trim_suffix function
    !---------------------------------------------------------------------------
    subroutine test_trim_suffix()
        character(len=:), allocatable :: result
        character(len=100) :: test_name
        
        print *, "Testing trim_suffix..."
        
        ! Test 1: String with matching suffix
        test_name = "trim_suffix with matching suffix"
        result = trim_suffix("hello_world", "_world")
        call assert_string_equal(test_name, result, "hello")
        
        ! Test 2: String without matching suffix
        test_name = "trim_suffix without matching suffix"
        result = trim_suffix("hello_world", "_earth")
        call assert_string_equal(test_name, result, "hello_world")
        
        ! Test 3: Empty string
        test_name = "trim_suffix with empty string"
        result = trim_suffix("", "_suffix")
        call assert_string_equal(test_name, result, "")
        
        ! Test 4: Empty suffix
        test_name = "trim_suffix with empty suffix"
        result = trim_suffix("hello", "")
        call assert_string_equal(test_name, result, "hello")
        
        ! Test 5: Suffix longer than string
        test_name = "trim_suffix with suffix longer than string"
        result = trim_suffix("hi", "_hello")
        call assert_string_equal(test_name, result, "hi")
        
        ! Test 6: Exact match
        test_name = "trim_suffix with exact match"
        result = trim_suffix("_suffix", "_suffix")
        call assert_string_equal(test_name, result, "")
        
    end subroutine test_trim_suffix
    
    !---------------------------------------------------------------------------
    ! Test get_outfile subroutine
    !---------------------------------------------------------------------------
    subroutine test_get_outfile()
        character(len=128) :: filename(2000,3)
        character(len=128) :: outfile_name(100)
        integer :: noutfile
        character(len=100) :: test_name
        
        print *, "Testing get_outfile..."
        
        ! Test 1: Basic test with unique filenames
        test_name = "get_outfile with unique filenames"
        filename = ''
        filename(1,1) = 'file1.nc'
        filename(2,1) = 'file2.nc'
        filename(1,2) = 'file3.nc'
        filename(1,3) = 'file4.nc'
        
        call get_outfile(3, filename, outfile_name, noutfile)
        call assert_equal(trim(test_name)//" count", noutfile, 4)
        call assert_string_equal(trim(test_name)//" file1", trim(outfile_name(1)), "file1.nc")
        call assert_string_equal(trim(test_name)//" file2", trim(outfile_name(2)), "file2.nc")
        call assert_string_equal(trim(test_name)//" file3", trim(outfile_name(3)), "file3.nc")
        call assert_string_equal(trim(test_name)//" file4", trim(outfile_name(4)), "file4.nc")
        
        ! Test 2: Test with duplicate filenames
        test_name = "get_outfile with duplicate filenames"
        filename = ''
        filename(1,1) = 'file1.nc'
        filename(2,1) = 'file2.nc'
        filename(1,2) = 'file1.nc'  ! duplicate
        filename(2,2) = 'file3.nc'
        
        call get_outfile(2, filename, outfile_name, noutfile)
        call assert_equal(trim(test_name)//" count", noutfile, 3)
        
        ! Test 3: Test with 'none' entries
        test_name = "get_outfile with 'none' entries"
        filename = ''
        filename(1,1) = 'file1.nc'
        filename(2,1) = 'none'
        filename(3,1) = 'file2.nc'
        
        call get_outfile(1, filename, outfile_name, noutfile)
        call assert_equal(trim(test_name)//" count", noutfile, 2)
        
        ! Test 4: Test with empty entries
        test_name = "get_outfile with empty entries"
        filename = ''
        filename(1,1) = 'file1.nc'
        filename(2,1) = ''
        filename(3,1) = 'file2.nc'  ! This won't be processed
        
        call get_outfile(1, filename, outfile_name, noutfile)
        call assert_equal(trim(test_name)//" count", noutfile, 1)
        
    end subroutine test_get_outfile
    
    !---------------------------------------------------------------------------
    ! Test lambert subroutine
    !---------------------------------------------------------------------------
    subroutine test_lambert()
        real(8) :: stlat1, stlat2, c_lat, c_lon
        real(8) :: glon, glat, x, y
        real(8) :: glon_out, glat_out, x_out, y_out
        real(8), parameter :: tol = 1.0e-6
        character(len=100) :: test_name
        
        print *, "Testing lambert..."
        
        ! Test 1: Forward transformation (glon,glat) -> (x,y)
        test_name = "lambert forward transformation"
        stlat1 = 30.0_8
        stlat2 = 60.0_8
        c_lat = 45.0_8
        c_lon = -100.0_8
        glon = -95.0_8
        glat = 40.0_8
        
        call lambert(stlat1, stlat2, c_lat, c_lon, glon, glat, x, y, 1)
        
        ! Test inverse transformation
        glon_out = 0.0_8
        glat_out = 0.0_8
        call lambert(stlat1, stlat2, c_lat, c_lon, glon_out, glat_out, x, y, -1)
        
        call assert_real_equal(trim(test_name)//" glon roundtrip", glon_out, glon, tol)
        call assert_real_equal(trim(test_name)//" glat roundtrip", glat_out, glat, tol)
        
        ! Test 2: Special case where stlat1 == stlat2
        test_name = "lambert with equal standard latitudes"
        stlat1 = 45.0_8
        stlat2 = 45.0_8
        c_lat = 45.0_8
        c_lon = -100.0_8
        glon = -90.0_8
        glat = 45.0_8
        
        call lambert(stlat1, stlat2, c_lat, c_lon, glon, glat, x, y, 1)
        call lambert(stlat1, stlat2, c_lat, c_lon, glon_out, glat_out, x, y, -1)
        
        call assert_real_equal(trim(test_name)//" glon", glon_out, glon, tol)
        call assert_real_equal(trim(test_name)//" glat", glat_out, glat, tol)
        
        ! Test 3: Point at projection center
        test_name = "lambert at projection center"
        glon = c_lon
        glat = c_lat
        
        call lambert(stlat1, stlat2, c_lat, c_lon, glon, glat, x, y, 1)
        call assert_real_equal(trim(test_name)//" x", x, 0.0_8, tol)
        
        ! Test 4: Test longitude wrapping
        test_name = "lambert with longitude wrapping"
        glon = 170.0_8
        glat = 45.0_8
        c_lon = -170.0_8
        
        call lambert(stlat1, stlat2, c_lat, c_lon, glon, glat, x, y, 1)
        call lambert(stlat1, stlat2, c_lat, c_lon, glon_out, glat_out, x, y, -1)
        
        ! Check that longitude is properly wrapped
        if (glon_out > 180.0_8) glon_out = glon_out - 360.0_8
        call assert_real_equal(trim(test_name)//" glon", glon_out, glon, tol)
        
    end subroutine test_lambert
    
    !---------------------------------------------------------------------------
    ! Test rtll subroutine
    !---------------------------------------------------------------------------
    subroutine test_rtll()
        real(8) :: tlmd, tphd, almd, aphd, tlm0d, tph0d
        real(8), parameter :: tol = 1.0e-8
        character(len=100) :: test_name
        
        print *, "Testing rtll..."
        
        ! Test 1: Basic rotation
        test_name = "rtll basic rotation"
        tlm0d = -100.0_8
        tph0d = 45.0_8
        tlmd = 0.0_8
        tphd = 0.0_8
        
        call rtll(tlmd, tphd, almd, aphd, tlm0d, tph0d)
        
        ! Verify result is reasonable
        call assert_range(trim(test_name)//" almd", almd, -180.0_8, 180.0_8)
        call assert_range(trim(test_name)//" aphd", aphd, -90.0_8, 90.0_8)
        
        ! Test 2: No rotation (identity)
        test_name = "rtll identity"
        tlm0d = 0.0_8
        tph0d = 90.0_8  ! North pole rotation
        tlmd = 45.0_8
        tphd = 30.0_8
        
        call rtll(tlmd, tphd, almd, aphd, tlm0d, tph0d)
        call assert_real_equal(trim(test_name)//" almd", almd, tlmd, tol)
        call assert_real_equal(trim(test_name)//" aphd", aphd, tphd, tol)
        
        ! Test 3: Longitude wrapping
        test_name = "rtll longitude wrapping"
        tlm0d = 170.0_8
        tph0d = 0.0_8
        tlmd = 20.0_8
        tphd = 0.0_8
        
        call rtll(tlmd, tphd, almd, aphd, tlm0d, tph0d)
        call assert_range(trim(test_name)//" almd wrapped", almd, -180.0_8, 180.0_8)
        
        ! Test 4: Extreme latitudes
        test_name = "rtll extreme latitudes"
        tlm0d = 0.0_8
        tph0d = 0.0_8
        tlmd = 0.0_8
        tphd = 89.9_8  ! Near pole
        
        call rtll(tlmd, tphd, almd, aphd, tlm0d, tph0d)
        call assert_range(trim(test_name)//" aphd", aphd, -90.0_8, 90.0_8)
        
    end subroutine test_rtll
    
    !---------------------------------------------------------------------------
    ! Test splat8 subroutine
    !---------------------------------------------------------------------------
    subroutine test_splat8()
        real(8), allocatable :: aslat(:)
        integer :: jmax
        real(8), parameter :: tol = 1.0e-12
        character(len=100) :: test_name
        integer :: j
        
        print *, "Testing splat8..."
        
        ! Test 1: Gaussian grid (idrt=4)
        test_name = "splat8 Gaussian grid"
        jmax = 8
        allocate(aslat(jmax))
        
        call splat8(4, jmax, aslat)
        
        ! Check properties of Gaussian latitudes
        ! Should be symmetric about equator
        do j = 1, jmax/2
            call assert_real_equal(trim(test_name)//" symmetry", aslat(j), -aslat(jmax+1-j), tol)
        end do
        
        ! Should be in descending order (from north to south)
        do j = 2, jmax
            call assert_true(trim(test_name)//" descending order", aslat(j-1) > aslat(j))
        end do
        
        ! Should be bounded by [-1, 1]
        do j = 1, jmax
            call assert_range(trim(test_name)//" bounds", aslat(j), -1.0_8, 1.0_8)
        end do
        
        deallocate(aslat)
        
        ! Test 2: Regular grid with poles (idrt=0)
        test_name = "splat8 regular grid with poles"
        jmax = 9  ! Odd number for equator
        allocate(aslat(jmax))
        
        call splat8(0, jmax, aslat)
        
        ! First point should be at north pole
        call assert_real_equal(trim(test_name)//" north pole", aslat(1), 1.0_8, tol)
        
        ! Last point should be at south pole
        call assert_real_equal(trim(test_name)//" south pole", aslat(jmax), -1.0_8, tol)
        
        ! Middle point should be at equator (for odd jmax)
        call assert_real_equal(trim(test_name)//" equator", aslat((jmax+1)/2), 0.0_8, tol)
        
        deallocate(aslat)
        
        ! Test 3: Regular grid without poles (idrt=256)
        test_name = "splat8 regular grid without poles"
        jmax = 8
        allocate(aslat(jmax))
        
        call splat8(256, jmax, aslat)
        
        ! First point should not be at pole
        call assert_true(trim(test_name)//" not at north pole", aslat(1) < 1.0_8)
        
        ! Last point should not be at pole
        call assert_true(trim(test_name)//" not at south pole", aslat(jmax) > -1.0_8)
        
        ! Should still be symmetric
        do j = 1, jmax/2
            call assert_real_equal(trim(test_name)//" symmetry", aslat(j), -aslat(jmax+1-j), tol)
        end do
        
        deallocate(aslat)
        
    end subroutine test_splat8
    
    !---------------------------------------------------------------------------
    ! Test splat4 subroutine
    !---------------------------------------------------------------------------
    subroutine test_splat4()
        real(4), allocatable :: aslat(:)
        real(8), allocatable :: aslat8(:)
        integer :: jmax
        real(4), parameter :: tol = 1.0e-6
        character(len=100) :: test_name
        integer :: j
        
        print *, "Testing splat4..."
        
        ! Test 1: Gaussian grid (idrt=4)
        test_name = "splat4 Gaussian grid"
        jmax = 8
        allocate(aslat(jmax))
        
        call splat4(4, jmax, aslat)
        
        ! Check properties of Gaussian latitudes
        ! Should be symmetric about equator
        do j = 1, jmax/2
            call assert_real4_equal(trim(test_name)//" symmetry", aslat(j), -aslat(jmax+1-j), tol)
        end do
        
        ! Should be in descending order (from north to south)
        do j = 2, jmax
            call assert_true(trim(test_name)//" descending order", aslat(j-1) > aslat(j))
        end do
        
        deallocate(aslat)
        
        ! Test 2: Regular grid with poles (idrt=0)
        test_name = "splat4 regular grid with poles"
        jmax = 9
        allocate(aslat(jmax))
        
        call splat4(0, jmax, aslat)
        
        ! First point should be at north pole
        call assert_real4_equal(trim(test_name)//" north pole", aslat(1), 1.0_4, tol)
        
        ! Last point should be at south pole
        call assert_real4_equal(trim(test_name)//" south pole", aslat(jmax), -1.0_4, tol)
        
        deallocate(aslat)
        
        ! Test 3: Comparison between splat4 and splat8
        test_name = "splat4 vs splat8 comparison"
        jmax = 16
        allocate(aslat(jmax))
        allocate(aslat8(jmax))
        
        call splat4(4, jmax, aslat)
        call splat8(4, jmax, aslat8)
        
        ! Results should be close (within single precision tolerance)
        do j = 1, jmax
            call assert_real4_equal(trim(test_name)//" consistency", aslat(j), real(aslat8(j),4), tol)
        end do
        
        deallocate(aslat)
        deallocate(aslat8)
        
    end subroutine test_splat4
    
    !---------------------------------------------------------------------------
    ! Assertion utilities
    !---------------------------------------------------------------------------
    subroutine assert_equal(test_name, actual, expected)
        character(len=*), intent(in) :: test_name
        integer, intent(in) :: actual, expected
        
        if (actual == expected) then
            print *, "PASS: ", trim(test_name)
            tests_passed = tests_passed + 1
        else
            print *, "FAIL: ", trim(test_name)
            print *, "  Expected: ", expected
            print *, "  Actual: ", actual
            tests_failed = tests_failed + 1
        end if
    end subroutine assert_equal
    
    subroutine assert_string_equal(test_name, actual, expected)
        character(len=*), intent(in) :: test_name
        character(len=*), intent(in) :: actual, expected
        
        if (actual == expected) then
            print *, "PASS: ", trim(test_name)
            tests_passed = tests_passed + 1
        else
            print *, "FAIL: ", trim(test_name)
            print *, "  Expected: '", trim(expected), "'"
            print *, "  Actual: '", trim(actual), "'"
            tests_failed = tests_failed + 1
        end if
    end subroutine assert_string_equal
    
    subroutine assert_real_equal(test_name, actual, expected, tolerance)
        character(len=*), intent(in) :: test_name
        real(8), intent(in) :: actual, expected, tolerance
        
        if (abs(actual - expected) <= tolerance) then
            print *, "PASS: ", trim(test_name)
            tests_passed = tests_passed + 1
        else
            print *, "FAIL: ", trim(test_name)
            print *, "  Expected: ", expected
            print *, "  Actual: ", actual
            print *, "  Difference: ", abs(actual - expected)
            tests_failed = tests_failed + 1
        end if
    end subroutine assert_real_equal
    
    subroutine assert_real4_equal(test_name, actual, expected, tolerance)
        character(len=*), intent(in) :: test_name
        real(4), intent(in) :: actual, expected, tolerance
        
        if (abs(actual - expected) <= tolerance) then
            print *, "PASS: ", trim(test_name)
            tests_passed = tests_passed + 1
        else
            print *, "FAIL: ", trim(test_name)
            print *, "  Expected: ", expected
            print *, "  Actual: ", actual
            print *, "  Difference: ", abs(actual - expected)
            tests_failed = tests_failed + 1
        end if
    end subroutine assert_real4_equal
    
    subroutine assert_true(test_name, condition)
        character(len=*), intent(in) :: test_name
        logical, intent(in) :: condition
        
        if (condition) then
            print *, "PASS: ", trim(test_name)
            tests_passed = tests_passed + 1
        else
            print *, "FAIL: ", trim(test_name)
            print *, "  Condition was false"
            tests_failed = tests_failed + 1
        end if
    end subroutine assert_true
    
    subroutine assert_range(test_name, value, min_val, max_val)
        character(len=*), intent(in) :: test_name
        real(8), intent(in) :: value, min_val, max_val
        
        if (value >= min_val .and. value <= max_val) then
            print *, "PASS: ", trim(test_name)
            tests_passed = tests_passed + 1
        else
            print *, "FAIL: ", trim(test_name)
            print *, "  Value: ", value
            print *, "  Expected range: [", min_val, ", ", max_val, "]"
            tests_failed = tests_failed + 1
        end if
    end subroutine assert_range
    
end program test_module_wrt_grid_comp
