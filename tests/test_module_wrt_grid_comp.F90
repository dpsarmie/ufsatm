program test_module_wrt_grid_comp
    use module_wrt_grid_comp, only: get_outfile, lambert, rtll, splat8, splat4
    implicit none

    real(8) :: x2,x3,x4,x5
    real(8) :: y2,y3,y4,y5
    real(8) :: glat_inv3, glon_inv3
  
    call test_get_outfile()
    call test_lambert()
    print *, "========================================="
    print *, x2, x3, x4, x5
    print *, y2, y3, y4, y5
    print *, glat_inv3, glon_inv3

    call test_rtll()
    call test_splat8()
    call test_splat4()

contains
    
    !---------------------------------------------------------------------------
    ! Test get_outfile subroutine
    !---------------------------------------------------------------------------
    subroutine test_get_outfile()
        character(len=128) :: filename(2000,3)
        character(len=128) :: outfile_name(100)
        integer :: noutfile
        character(len=100) :: test_name
        
        ! Test 1: Basic test with unique filenames
        filename = ''
        filename(1,1) = 'file1.nc'
        filename(2,1) = 'file2.nc'
        filename(1,2) = 'file3.nc'
        filename(1,3) = 'file4.nc'
        
        call get_outfile(3, filename, outfile_name, noutfile)
        if ( trim(outfile_name(1)) /= "file1.nc" .or. &
             trim(outfile_name(2)) /= "file2.nc" .or. &
             trim(outfile_name(3)) /= "file3.nc" .or. &
             trim(outfile_name(4)) /= "file4.nc") then
             
            stop 1
            
        end if
        
    end subroutine test_get_outfile
    
    !---------------------------------------------------------------------------
    ! Test lambert subroutine
    !---------------------------------------------------------------------------
    subroutine test_lambert()
        real(8) :: stlat1, stlat2, c_lat, c_lon
        real(8) :: glon, glat, x, y
        real(8) :: glon_inv, glat_inv, x_out, y_out
        real(8) :: true_x, true_y
        real(8), parameter :: tol = 1.0e1 ! Difference tolerance
        character(len=100) :: test_name
        
        ! Test 1: Forward transformation (glon,glat) -> (x,y)
        ! National mall, Washington DC
        stlat1 = 38.5_8
        stlat2 = 39.5_8
        c_lat = 39.0_8
        c_lon = -77.0_8
        glon = -77.0353_8
        glat = 38.8895_8
        
        true_x = -3055.18
        true_y = -12286.37
        call lambert(stlat1, stlat2, c_lat, c_lon, glon, glat, x, y, 1)

        x2 = x
        y2 = y

        if ( (true_x - x) > tol .or. (true_y - y) > tol ) then
          print *, x, y
          !stop 2
        end if          
              
        ! Test inverse transformation
        glon_inv = 0.0_8
        glat_inv = 0.0_8
        call lambert(stlat1, stlat2, c_lat, c_lon, glon_inv, glat_inv, x, y, -1)

        glat_inv3 = glat_inv
        glon_inv3 = glon_inv
        x3 = x
        y3 = y

        if ( (glon - glon_inv) > tol .or. (glat - glat_inv) > tol ) then
          print *, glon_inv, glat_inv
          !stop 3
        end if
        
        ! Test 2: Special case where stlat1 == stlat2
        ! Italy
        stlat1 = 45.5_8
        stlat2 = 45.5_8
        c_lat = 45.5_8
        c_lon = 11.0_8
        glon = 45.4642_8
        glat = 9.1900_8
        
        true_x = -141149.15
        true_y = -2390.66
        
        call lambert(stlat1, stlat2, c_lat, c_lon, glon, glat, x, y, 1)

        x4 = x
        y4 = y

        if ( (true_x - x) > tol .or. (true_y - y) > tol ) then
          print *, x, y
          !stop 4
        end if
        
        ! Test 3: Point at projection center
        test_name = "lambert at projection center"
        glon = c_lon
        glat = c_lat
        
        call lambert(stlat1, stlat2, c_lat, c_lon, glon, glat, x, y, 1)

        x5 = x
        y5 = y
      
        if ( true_x /= x .or. true_y /= y ) then
          print *, x, y
          !stop 5
        end if
        
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
        else
            print *, "FAIL: ", trim(test_name)
            print *, "  Expected: ", expected
            print *, "  Actual: ", actual
        end if
    end subroutine assert_equal
    
    subroutine assert_string_equal(test_name, actual, expected)
        character(len=*), intent(in) :: test_name
        character(len=*), intent(in) :: actual, expected
        
        if (actual == expected) then
            print *, "PASS: ", trim(test_name)
        else
            print *, "FAIL: ", trim(test_name)
            print *, "  Expected: '", trim(expected), "'"
            print *, "  Actual: '", trim(actual), "'"
        end if
    end subroutine assert_string_equal
    
    subroutine assert_real_equal(test_name, actual, expected, tolerance)
        character(len=*), intent(in) :: test_name
        real(8), intent(in) :: actual, expected, tolerance
        
        if (abs(actual - expected) <= tolerance) then
            print *, "PASS: ", trim(test_name)
        else
            print *, "FAIL: ", trim(test_name)
            print *, "  Expected: ", expected
            print *, "  Actual: ", actual
            print *, "  Difference: ", abs(actual - expected)
        end if
    end subroutine assert_real_equal
    
    subroutine assert_real4_equal(test_name, actual, expected, tolerance)
        character(len=*), intent(in) :: test_name
        real(4), intent(in) :: actual, expected, tolerance
        
        if (abs(actual - expected) <= tolerance) then
            print *, "PASS: ", trim(test_name)
        else
            print *, "FAIL: ", trim(test_name)
            print *, "  Expected: ", expected
            print *, "  Actual: ", actual
            print *, "  Difference: ", abs(actual - expected)
        end if
    end subroutine assert_real4_equal
    
    subroutine assert_true(test_name, condition)
        character(len=*), intent(in) :: test_name
        logical, intent(in) :: condition
        
        if (condition) then
            print *, "PASS: ", trim(test_name)
        else
            print *, "FAIL: ", trim(test_name)
            print *, "  Condition was false"
        end if
    end subroutine assert_true
    
    subroutine assert_range(test_name, value, min_val, max_val)
        character(len=*), intent(in) :: test_name
        real(8), intent(in) :: value, min_val, max_val
        
        if (value >= min_val .and. value <= max_val) then
            print *, "PASS: ", trim(test_name)
        else
            print *, "FAIL: ", trim(test_name)
            print *, "  Value: ", value
            print *, "  Expected range: [", min_val, ", ", max_val, "]"
        end if
    end subroutine assert_range
    
end program test_module_wrt_grid_comp
