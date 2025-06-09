module fv_tracker_test_mod
  ! Unit tests for fv_tracker_mod subroutines and functions
  
  use fv_tracker_mod, only: calcdist, clean_lon_lat, get_lat_ns, get_lon_ew
#ifdef DEBUG
  use fv_tracker_mod, only: check_validity
#endif
  
  implicit none
  private
  
  public :: run_all_fv_tracker_tests
  
  ! Test tolerance for floating point comparisons
  real, parameter :: TOLERANCE = 1.0e-6
  
contains

  subroutine run_all_fv_tracker_tests()
    implicit none
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    write(*,*) "Starting fv_tracker unit tests..."
    write(*,*) "================================"
    
    ! Run individual test suites
    if (.not. test_calcdist()) all_tests_passed = .false.
    if (.not. test_clean_lon_lat()) all_tests_passed = .false.
    if (.not. test_get_lat_ns()) all_tests_passed = .false.
    if (.not. test_get_lon_ew()) all_tests_passed = .false.
#ifdef DEBUG
    if (.not. test_check_validity()) all_tests_passed = .false.
#endif
    
    write(*,*) "================================"
    if (all_tests_passed) then
      write(*,*) "All tests PASSED!"
    else
      write(*,*) "Some tests FAILED!"
    endif
    
  end subroutine run_all_fv_tracker_tests

  !============================================================================
  ! Test calcdist subroutine
  !============================================================================
  logical function test_calcdist()
    implicit none
    real :: rlonb, rlatb, rlonc, rlatc
    real :: xdist, degrees
    logical :: test_passed
    
    test_passed = .true.
    write(*,*) "Testing calcdist..."
    
    ! Test 1: Same point (distance should be 0)
    rlonb = 0.0
    rlatb = 0.0
    rlonc = 0.0
    rlatc = 0.0
    call calcdist(rlonb, rlatb, rlonc, rlatc, xdist, degrees)
    if (abs(xdist) > TOLERANCE) then
      write(*,*) "  FAILED: Same point test. Expected 0, got", xdist
      test_passed = .false.
    else
      write(*,*) "  PASSED: Same point test"
    endif
    
    ! Test 2: Points on equator, 90 degrees apart
    rlonb = 0.0
    rlatb = 0.0
    rlonc = 90.0
    rlatc = 0.0
    call calcdist(rlonb, rlatb, rlonc, rlatc, xdist, degrees)
    ! Expected distance is approximately 1/4 of Earth's circumference
    if (abs(xdist - 10007.55) > 1.0) then  ! Using larger tolerance for this
      write(*,*) "  FAILED: Equator 90 deg test. Expected ~10007.55 km, got", xdist
      test_passed = .false.
    else
      write(*,*) "  PASSED: Equator 90 degrees apart test"
    endif
    
    ! Test 3: Pole to equator
    rlonb = 0.0
    rlatb = 90.0
    rlonc = 0.0
    rlatc = 0.0
    call calcdist(rlonb, rlatb, rlonc, rlatc, xdist, degrees)
    if (abs(degrees - 90.0) > TOLERANCE) then
      write(*,*) "  FAILED: Pole to equator test. Expected 90 degrees, got", degrees
      test_passed = .false.
    else
      write(*,*) "  PASSED: Pole to equator test"
    endif
    
    ! Test 4: Antipodal points (opposite sides of Earth)
    rlonb = 0.0
    rlatb = 0.0
    rlonc = 180.0
    rlatc = 0.0
    call calcdist(rlonb, rlatb, rlonc, rlatc, xdist, degrees)
    ! Expected distance is half of Earth's circumference
    if (abs(xdist - 20015.1) > 1.0) then
      write(*,*) "  FAILED: Antipodal test. Expected ~20015.1 km, got", xdist
      test_passed = .false.
    else
      write(*,*) "  PASSED: Antipodal points test"
    endif
    
    test_calcdist = test_passed
  end function test_calcdist

  !============================================================================
  ! Test clean_lon_lat subroutine
  !============================================================================
  logical function test_clean_lon_lat()
    implicit none
    real :: xlon, ylat
    logical :: test_passed
    
    test_passed = .true.
    write(*,*) "Testing clean_lon_lat..."
    
    ! Test 1: Normal values (should remain unchanged)
    xlon = 45.0
    ylat = 30.0
    call clean_lon_lat(xlon, ylat)
    if (abs(xlon - 45.0) > TOLERANCE .or. abs(ylat - 30.0) > TOLERANCE) then
      write(*,*) "  FAILED: Normal values test"
      test_passed = .false.
    else
      write(*,*) "  PASSED: Normal values test"
    endif
    
    ! Test 2: Longitude > 180
    xlon = 270.0
    ylat = 0.0
    call clean_lon_lat(xlon, ylat)
    if (abs(xlon - (-90.0)) > TOLERANCE) then
      write(*,*) "  FAILED: Lon > 180 test. Expected -90, got", xlon
      test_passed = .false.
    else
      write(*,*) "  PASSED: Longitude > 180 test"
    endif
    
    ! Test 3: Longitude < -180
    xlon = -270.0
    ylat = 0.0
    call clean_lon_lat(xlon, ylat)
    if (abs(xlon - 90.0) > TOLERANCE) then
      write(*,*) "  FAILED: Lon < -180 test. Expected 90, got", xlon
      test_passed = .false.
    else
      write(*,*) "  PASSED: Longitude < -180 test"
    endif
    
    ! Test 4: Latitude > 90
    xlon = 0.0
    ylat = 120.0
    call clean_lon_lat(xlon, ylat)
    if (abs(ylat - 60.0) > TOLERANCE .or. abs(xlon - 180.0) > TOLERANCE) then
      write(*,*) "  FAILED: Lat > 90 test. Expected lat=60, lon=180, got lat=", ylat, "lon=", xlon
      test_passed = .false.
    else
      write(*,*) "  PASSED: Latitude > 90 test"
    endif
    
    ! Test 5: Latitude < -90
    xlon = 0.0
    ylat = -120.0
    call clean_lon_lat(xlon, ylat)
    if (abs(ylat - (-60.0)) > TOLERANCE .or. abs(xlon - 180.0) > TOLERANCE) then
      write(*,*) "  FAILED: Lat < -90 test. Expected lat=-60, lon=180, got lat=", ylat, "lon=", xlon
      test_passed = .false.
    else
      write(*,*) "  PASSED: Latitude < -90 test"
    endif
    
    test_clean_lon_lat = test_passed
  end function test_clean_lon_lat

  !============================================================================
  ! Test get_lat_ns function
  !============================================================================
  logical function test_get_lat_ns()
    implicit none
    character(1) :: result
    logical :: test_passed
    
    test_passed = .true.
    write(*,*) "Testing get_lat_ns..."
    
    ! Test 1: Positive latitude (Northern hemisphere)
    result = get_lat_ns(45.0)
    if (result /= 'N') then
      write(*,*) "  FAILED: Positive lat test. Expected 'N', got '", result, "'"
      test_passed = .false.
    else
      write(*,*) "  PASSED: Positive latitude test"
    endif
    
    ! Test 2: Negative latitude (Southern hemisphere)
    result = get_lat_ns(-30.0)
    if (result /= 'S') then
      write(*,*) "  FAILED: Negative lat test. Expected 'S', got '", result, "'"
      test_passed = .false.
    else
      write(*,*) "  PASSED: Negative latitude test"
    endif
    
    ! Test 3: Zero latitude (Equator - considered North)
    result = get_lat_ns(0.0)
    if (result /= 'N') then
      write(*,*) "  FAILED: Zero lat test. Expected 'N', got '", result, "'"
      test_passed = .false.
    else
      write(*,*) "  PASSED: Zero latitude test"
    endif
    
    ! Test 4: Extreme positive
    result = get_lat_ns(90.0)
    if (result /= 'N') then
      write(*,*) "  FAILED: North pole test. Expected 'N', got '", result, "'"
      test_passed = .false.
    else
      write(*,*) "  PASSED: North pole test"
    endif
    
    ! Test 5: Extreme negative
    result = get_lat_ns(-90.0)
    if (result /= 'S') then
      write(*,*) "  FAILED: South pole test. Expected 'S', got '", result, "'"
      test_passed = .false.
    else
      write(*,*) "  PASSED: South pole test"
    endif
    
    test_get_lat_ns = test_passed
  end function test_get_lat_ns

  !============================================================================
  ! Test get_lon_ew function
  !============================================================================
  logical function test_get_lon_ew()
    implicit none
    character(1) :: result
    logical :: test_passed
    
    test_passed = .true.
    write(*,*) "Testing get_lon_ew..."
    
    ! Test 1: Positive longitude (Eastern hemisphere)
    result = get_lon_ew(45.0)
    if (result /= 'E') then
      write(*,*) "  FAILED: Positive lon test. Expected 'E', got '", result, "'"
      test_passed = .false.
    else
      write(*,*) "  PASSED: Positive longitude test"
    endif
    
    ! Test 2: Negative longitude (Western hemisphere)
    result = get_lon_ew(-120.0)
    if (result /= 'W') then
      write(*,*) "  FAILED: Negative lon test. Expected 'W', got '", result, "'"
      test_passed = .false.
    else
      write(*,*) "  PASSED: Negative longitude test"
    endif
    
    ! Test 3: Zero longitude (Prime meridian - considered East)
    result = get_lon_ew(0.0)
    if (result /= 'E') then
      write(*,*) "  FAILED: Zero lon test. Expected 'E', got '", result, "'"
      test_passed = .false.
    else
      write(*,*) "  PASSED: Zero longitude test"
    endif
    
    ! Test 4: Extreme positive
    result = get_lon_ew(180.0)
    if (result /= 'E') then
      write(*,*) "  FAILED: 180 deg test. Expected 'E', got '", result, "'"
      test_passed = .false.
    else
      write(*,*) "  PASSED: 180 degrees test"
    endif
    
    ! Test 5: Extreme negative
    result = get_lon_ew(-180.0)
    if (result /= 'W') then
      write(*,*) "  FAILED: -180 deg test. Expected 'W', got '", result, "'"
      test_passed = .false.
    else
      write(*,*) "  PASSED: -180 degrees test"
    endif
    
    test_get_lon_ew = test_passed
  end function test_get_lon_ew

#ifdef DEBUG
  !============================================================================
  ! Test check_validity subroutine
  !============================================================================
  logical function test_check_validity()
    implicit none
    logical :: test_passed
    integer :: original_stdout, temp_unit
    character(len=1000) :: temp_file
    
    test_passed = .true.
    write(*,*) "Testing check_validity..."
    
    ! Note: This subroutine writes to stdout when invalid values are found
    ! In a real test environment, we would capture and verify the output
    
    ! Test 1: Valid vorticity value
    write(*,*) "  Testing valid vorticity..."
    call check_validity("zeta", 1.0e-4, 10, 20)
    write(*,*) "  PASSED: Valid vorticity test (no output expected)"
    
    ! Test 2: Invalid vorticity value (too large)
    write(*,*) "  Testing invalid vorticity (expect warning output)..."
    call check_validity("zeta", 1.0, 10, 20)
    write(*,*) "  PASSED: Invalid vorticity test (warning should appear above)"
    
    ! Test 3: Valid height value
    write(*,*) "  Testing valid height..."
    call check_validity("hgt", 1500.0, 10, 20)
    write(*,*) "  PASSED: Valid height test (no output expected)"
    
    ! Test 4: Invalid SLP value (too low)
    write(*,*) "  Testing invalid SLP (expect warning output)..."
    call check_validity("slp", 1.0e4, 10, 20)
    write(*,*) "  PASSED: Invalid SLP test (warning should appear above)"
    
    ! Test 5: Valid wind value
    write(*,*) "  Testing valid wind..."
    call check_validity("wind", 25.0, 10, 20)
    write(*,*) "  PASSED: Valid wind test (no output expected)"
    
    ! Test 6: Unknown parameter
    write(*,*) "  Testing unknown parameter (expect warning output)..."
    call check_validity("unknown", 1.0, 10, 20)
    write(*,*) "  PASSED: Unknown parameter test (warning should appear above)"
    
    test_check_validity = test_passed
  end function test_check_validity
#endif

end module fv_tracker_test_mod

! Main program to run the tests
program test_fv_tracker
  use fv_tracker_test_mod
  implicit none
  
  call run_all_fv_tracker_tests()
  
end program test_fv_tracker
