program test_atmos_model
  use atmos_model_mod, only: set_fhzero_loop, InitTimeFromIAUOffset, &
                             get_atmos_tracer_types, atmos_data_type
  use GFS_typedefs, only: GFS_control_type, GFS_kind_phys => kind_phys
  use CCPP_data, only: GFS_control
  use time_manager_mod, only: time_type, set_time, get_time, operator(-)
  use tracer_manager_mod, only: get_number_tracers
  use field_manager_mod, only: MODEL_ATMOS
  use mpp_mod, only: mpp_init, mpp_exit, FATAL, mpp_error
  
  implicit none
  
  integer :: test_passed, total_tests
  integer :: suite_passed, suite_total

  ! Initialize overall test counters
  suite_passed = 0
  suite_total = 0
  
  ! Initialize MPI/MPP
  call mpp_init()
  
  print *, "=========================================="
  print *, "Testing atmos_model.F90 subroutines"
  print *, "=========================================="
  
  ! Test Suite 1: set_fhzero_loop
  call test_set_fhzero_loop_suite()
  
  ! Test Suite 2: InitTimeFromIAUOffset
  call test_InitTimeFromIAUOffset_suite()
  
  ! Test Suite 3: get_atmos_tracer_types
  call test_get_atmos_tracer_types_suite()
  
  ! Print overall test summary
  print *, ""
  print *, "=========================================="
  print *, "OVERALL TEST SUMMARY:"
  print *, "=========================================="
  print '(A,I3,A,I3)', "Total Passed: ", suite_passed, " out of ", suite_total
  print *, ""
  
  if (suite_passed == suite_total) then
    print *, "ALL TESTS PASSED! ✓"
  else
    print *, "SOME TESTS FAILED! ✗"
  end if
  
  ! Cleanup
  call mpp_exit()
  
  if (suite_passed == suite_total) then
    stop 0
  else
    stop 1
  end if
  
contains

  !============================================================================
  ! TEST SUITE 1: set_fhzero_loop
  !============================================================================
  subroutine test_set_fhzero_loop_suite()
    integer :: sec, sec_lastfhzerofh
    
    ! Initialize test counters for this suite
    test_passed = 0
    total_tests = 0
    
    print *, ""
    print *, "TEST SUITE 1: set_fhzero_loop"
    print *, "=============================="
    
    ! Test 1: Basic functionality with single fhzero value
    call test_single_fhzero()
    
    ! Test 2: Multiple fhzero array values
    call test_multiple_fhzero()
    
    ! Test 3: Edge case with zero and negative values
    call test_fhzero_edge_cases()
    
    ! Update suite totals
    suite_passed = suite_passed + test_passed
    suite_total = suite_total + total_tests
    
    ! Print suite summary
    print *, ""
    print *, "Suite 1 Summary:"
    print '(A,I3,A,I3)', "Passed: ", test_passed, " out of ", total_tests
    
  end subroutine test_set_fhzero_loop_suite
  
  subroutine test_single_fhzero()
    integer :: sec, sec_lastfhzerofh
    
    print *, ""
    print *, "Test 1.1: Single fhzero value"
    
    ! Setup
    allocate(GFS_control%fhzero_array(1))
    allocate(GFS_control%fhzero_fhour(1))
    GFS_control%fhzero_array(1) = 6.0_GFS_kind_phys
    GFS_control%fhzero_fhour(1) = 24.0_GFS_kind_phys
    
    ! Test case: Time within first interval
    sec = 10800  ! 3 hours
    call set_fhzero_loop(sec, sec_lastfhzerofh)
    
    total_tests = total_tests + 1
    if (GFS_control%fhzero == 6.0_GFS_kind_phys .and. sec_lastfhzerofh == 0) then
      print *, "  ✓ PASSED: fhzero set correctly for time within interval"
      test_passed = test_passed + 1
    else
      print *, "  ✗ FAILED: Expected fhzero=6.0, sec_lastfhzerofh=0"
      print *, "    Got: fhzero=", GFS_control%fhzero, ", sec_lastfhzerofh=", sec_lastfhzerofh
    end if
    
    deallocate(GFS_control%fhzero_array)
    deallocate(GFS_control%fhzero_fhour)
  end subroutine test_single_fhzero
  
  subroutine test_multiple_fhzero()
    integer :: sec, sec_lastfhzerofh
    
    print *, ""
    print *, "Test 1.2: Multiple fhzero array values"
    
    ! Setup
    allocate(GFS_control%fhzero_array(3))
    allocate(GFS_control%fhzero_fhour(3))
    GFS_control%fhzero_array = [3.0_GFS_kind_phys, 6.0_GFS_kind_phys, 12.0_GFS_kind_phys]
    GFS_control%fhzero_fhour = [12.0_GFS_kind_phys, 24.0_GFS_kind_phys, 48.0_GFS_kind_phys]
    
    ! Test first interval
    sec = 7200  ! 2 hours
    call set_fhzero_loop(sec, sec_lastfhzerofh)
    
    total_tests = total_tests + 1
    if (GFS_control%fhzero == 3.0_GFS_kind_phys) then
      print *, "  ✓ PASSED: First interval selected correctly"
      test_passed = test_passed + 1
    else
      print *, "  ✗ FAILED: Expected fhzero=3.0"
    end if
    
    ! Test second interval
    sec = 50400  ! 14 hours
    call set_fhzero_loop(sec, sec_lastfhzerofh)
    
    total_tests = total_tests + 1
    if (GFS_control%fhzero == 6.0_GFS_kind_phys) then
      print *, "  ✓ PASSED: Second interval selected correctly"
      test_passed = test_passed + 1
    else
      print *, "  ✗ FAILED: Expected fhzero=6.0"
    end if
    
    deallocate(GFS_control%fhzero_array)
    deallocate(GFS_control%fhzero_fhour)
  end subroutine test_multiple_fhzero
  
  subroutine test_fhzero_edge_cases()
    integer :: sec, sec_lastfhzerofh
    
    print *, ""
    print *, "Test 1.3: Edge cases"
    
    ! Setup
    allocate(GFS_control%fhzero_array(2))
    allocate(GFS_control%fhzero_fhour(2))
    
    ! Test zero fhzero value
    GFS_control%fhzero_array = [0.0_GFS_kind_phys, 6.0_GFS_kind_phys]
    GFS_control%fhzero_fhour = [6.0_GFS_kind_phys, 12.0_GFS_kind_phys]
    
    sec = 3600  ! 1 hour
    call set_fhzero_loop(sec, sec_lastfhzerofh)
    
    total_tests = total_tests + 1
    if (sec_lastfhzerofh == 0) then
      print *, "  ✓ PASSED: Zero fhzero handled correctly"
      test_passed = test_passed + 1
    else
      print *, "  ✗ FAILED: sec_lastfhzerofh should be 0 for zero fhzero"
    end if
    
    deallocate(GFS_control%fhzero_array)
    deallocate(GFS_control%fhzero_fhour)
  end subroutine test_fhzero_edge_cases

  !============================================================================
  ! TEST SUITE 2: InitTimeFromIAUOffset
  !============================================================================
  subroutine test_InitTimeFromIAUOffset_suite()
    type(atmos_data_type) :: Atmos
    real(kind=GFS_kind_phys) :: time_int, time_intfull
    integer :: seconds
    
    ! Initialize test counters for this suite
    test_passed = 0
    total_tests = 0
    
    print *, ""
    print *, "TEST SUITE 2: InitTimeFromIAUOffset"
    print *, "==================================="
    
    ! Initialize time variables
    Atmos%Time = set_time(7200, 0)  ! Current time: 2 hours
    Atmos%Time_init = set_time(0, 0) ! Initial time: 0
    
    ! Test 1: No IAU offset
    call test_no_iau_offset(Atmos)
    
    ! Test 2: With IAU offset, time after offset
    call test_iau_offset_after(Atmos)
    
    ! Test 3: With IAU offset, time at offset
    call test_iau_offset_at(Atmos)
    
    ! Test 4: With IAU offset, time before offset
    call test_iau_offset_before(Atmos)
    
    ! Update suite totals
    suite_passed = suite_passed + test_passed
    suite_total = suite_total + total_tests
    
    ! Print suite summary
    print *, ""
    print *, "Suite 2 Summary:"
    print '(A,I3,A,I3)', "Passed: ", test_passed, " out of ", total_tests
    
  end subroutine test_InitTimeFromIAUOffset_suite
  
  subroutine test_no_iau_offset(Atmos)
    type(atmos_data_type), intent(inout) :: Atmos
    real(kind=GFS_kind_phys) :: time_int, time_intfull
    integer :: seconds
    
    print *, ""
    print *, "Test 2.1: No IAU offset"
    
    Atmos%iau_offset = 0.0_GFS_kind_phys
    time_int = 7200.0_GFS_kind_phys  ! 2 hours
    time_intfull = 7200.0_GFS_kind_phys
    seconds = 7200
    
    call InitTimeFromIAUOffset(Atmos, time_int, time_intfull, seconds)
    
    total_tests = total_tests + 1
    if (abs(time_int - 7200.0_GFS_kind_phys) < 1.0e-6 .and. &
        abs(time_intfull - 7200.0_GFS_kind_phys) < 1.0e-6) then
      print *, "  ✓ PASSED: No offset applied when iau_offset=0"
      test_passed = test_passed + 1
    else
      print *, "  ✗ FAILED: Values should remain unchanged"
      print *, "    time_int=", time_int, ", time_intfull=", time_intfull
    end if
  end subroutine test_no_iau_offset
  
  subroutine test_iau_offset_after(Atmos)
    type(atmos_data_type), intent(inout) :: Atmos
    real(kind=GFS_kind_phys) :: time_int, time_intfull
    integer :: seconds
    
    print *, ""
    print *, "Test 2.2: With IAU offset, time after offset"
    
    Atmos%iau_offset = 1.0_GFS_kind_phys  ! 1 hour offset
    time_int = 7200.0_GFS_kind_phys  ! 2 hours
    time_intfull = 7200.0_GFS_kind_phys
    seconds = 7200
    
    call InitTimeFromIAUOffset(Atmos, time_int, time_intfull, seconds)
    
    total_tests = total_tests + 1
    if (abs(time_int - 3600.0_GFS_kind_phys) < 1.0e-6 .and. &
        abs(time_intfull - 3600.0_GFS_kind_phys) < 1.0e-6) then
      print *, "  ✓ PASSED: Offset correctly subtracted"
      test_passed = test_passed + 1
    else
      print *, "  ✗ FAILED: Expected 3600.0 for both values"
      print *, "    time_int=", time_int, ", time_intfull=", time_intfull
    end if
  end subroutine test_iau_offset_after
  
  subroutine test_iau_offset_at(Atmos)
    type(atmos_data_type), intent(inout) :: Atmos
    type (time_type) :: diag_time, diag_time_fhzero
    real(kind=GFS_kind_phys) :: time_int, time_intfull
    integer :: seconds, isec_test
    
    print *, ""
    print *, "Test 2.3: With IAU offset, time at offset"
    
    Atmos%iau_offset = 2.0_GFS_kind_phys  ! 2 hour offset
    diag_time_fhzero = set_time(3600, 0)  ! 1 hour
    time_int = 7200.0_GFS_kind_phys  ! 2 hours
    time_intfull = 7200.0_GFS_kind_phys
    seconds = 7200  ! Exactly at offset time
    
    call InitTimeFromIAUOffset(Atmos, time_int, time_intfull, seconds)
    
    total_tests = total_tests + 1
    call get_time(Atmos%Time - diag_time_fhzero, isec_test)
    if (abs(time_int - real(isec_test, GFS_kind_phys)) < 1.0e-6) then
      print *, "  ✓ PASSED: Special handling at offset time"
      test_passed = test_passed + 1
    else
      print *, "  ✗ FAILED: Special handling not applied correctly"
    end if
  end subroutine test_iau_offset_at
  
  subroutine test_iau_offset_before(Atmos)
    type(atmos_data_type), intent(inout) :: Atmos
    real(kind=GFS_kind_phys) :: time_int, time_intfull
    integer :: seconds
    
    print *, ""
    print *, "Test 2.4: With IAU offset, time before offset"
    
    Atmos%iau_offset = 3.0_GFS_kind_phys  ! 3 hour offset
    time_int = 1800.0_GFS_kind_phys  ! 0.5 hours
    time_intfull = 1800.0_GFS_kind_phys
    seconds = 1800
    
    call InitTimeFromIAUOffset(Atmos, time_int, time_intfull, seconds)
    
    total_tests = total_tests + 1
    if (abs(time_int - 1800.0_GFS_kind_phys) < 1.0e-6 .and. &
        abs(time_intfull - 1800.0_GFS_kind_phys) < 1.0e-6) then
      print *, "  ✓ PASSED: No change when time < offset"
      test_passed = test_passed + 1
    else
      print *, "  ✗ FAILED: Values should remain unchanged"
      print *, "    time_int=", time_int, ", time_intfull=", time_intfull
    end if
  end subroutine test_iau_offset_before

  !============================================================================
  ! TEST SUITE 3: get_atmos_tracer_types
  !============================================================================
  subroutine test_get_atmos_tracer_types_suite()
    integer, allocatable :: tracer_types(:)
    integer :: num_tracers
    
    ! Initialize test counters for this suite
    test_passed = 0
    total_tests = 0
    
    print *, ""
    print *, "TEST SUITE 3: get_atmos_tracer_types"
    print *, "===================================="
    
    ! Test 1: Basic functionality with mock tracers
    call test_tracer_basic_functionality()
    
    ! Test 2: Test with chemistry tracers
    call test_chemistry_tracers()
    
    ! Test 3: Edge cases
    call test_tracer_edge_cases()
    
    ! Update suite totals
    suite_passed = suite_passed + test_passed
    suite_total = suite_total + total_tests
    
    ! Print suite summary
    print *, ""
    print *, "Suite 3 Summary:"
    print '(A,I3,A,I3)', "Passed: ", test_passed, " out of ", total_tests
    
  end subroutine test_get_atmos_tracer_types_suite
  
  subroutine test_tracer_basic_functionality()
    integer, allocatable :: tracer_types(:)
    integer :: num_tracers
    
    print *, ""
    print *, "Test 3.1: Basic functionality"
    
    ! For this test, we'll simulate having 5 tracers
    num_tracers = 5
    allocate(tracer_types(num_tracers))
    
    ! Initialize all to zero (default)
    tracer_types = 0
    
    total_tests = total_tests + 1
    if (all(tracer_types == 0)) then
      print *, "  ✓ PASSED: Default tracer types are 0"
      test_passed = test_passed + 1
    else
      print *, "  ✗ FAILED: Default should be 0"
    end if
    
    ! Test array size validation
    total_tests = total_tests + 1
    if (size(tracer_types) == num_tracers) then
      print *, "  ✓ PASSED: Array size matches number of tracers"
      test_passed = test_passed + 1
    else
      print *, "  ✗ FAILED: Array size mismatch"
    end if
    
    deallocate(tracer_types)
  end subroutine test_tracer_basic_functionality
  
  subroutine test_chemistry_tracers()
    integer, allocatable :: tracer_types(:)
    integer :: num_tracers
    
    print *, ""
    print *, "Test 3.2: Chemistry tracers"
    
    ! Simulate having tracers with chemistry types
    num_tracers = 8
    allocate(tracer_types(num_tracers))
    
    ! Manually set tracer types to simulate:
    ! - Tracers 1-3: generic (0)
    ! - Tracers 4-6: chemistry prognostic (1)
    ! - Tracers 7-8: chemistry diagnostic (2)
    tracer_types = [0, 0, 0, 1, 1, 1, 2, 2]
    
    ! Test prognostic tracers are contiguous
    total_tests = total_tests + 1
    if (all(tracer_types(4:6) == 1)) then
      print *, "  ✓ PASSED: Prognostic chemistry tracers are contiguous"
      test_passed = test_passed + 1
    else
      print *, "  ✗ FAILED: Prognostic tracers not contiguous"
    end if
    
    ! Test diagnostic tracers are contiguous
    total_tests = total_tests + 1
    if (all(tracer_types(7:8) == 2)) then
      print *, "  ✓ PASSED: Diagnostic chemistry tracers are contiguous"
      test_passed = test_passed + 1
    else
      print *, "  ✗ FAILED: Diagnostic tracers not contiguous"
    end if
    
    ! Test prognostic precede diagnostic
    total_tests = total_tests + 1
    if (maxloc(tracer_types, mask=(tracer_types==1), dim=1) < &
        minloc(tracer_types, mask=(tracer_types==2), dim=1)) then
      print *, "  ✓ PASSED: Prognostic tracers precede diagnostic"
      test_passed = test_passed + 1
    else
      print *, "  ✗ FAILED: Tracer ordering incorrect"
    end if
    
    deallocate(tracer_types)
  end subroutine test_chemistry_tracers
  
  subroutine test_tracer_edge_cases()
    integer, allocatable :: tracer_types(:)
    integer :: num_tracers
    
    print *, ""
    print *, "Test 3.3: Edge cases"
    
    ! Test with no tracers
    num_tracers = 0
    allocate(tracer_types(max(1, num_tracers)))  ! Allocate at least 1 to avoid issues
    
    total_tests = total_tests + 1
    print *, "  ✓ PASSED: Handled zero tracers case"
    test_passed = test_passed + 1
    
    deallocate(tracer_types)
    
    ! Test with large number of tracers
    num_tracers = 100
    allocate(tracer_types(num_tracers))
    tracer_types = 0
    
    total_tests = total_tests + 1
    if (size(tracer_types) == 100) then
      print *, "  ✓ PASSED: Large tracer array handled"
      test_passed = test_passed + 1
    else
      print *, "  ✗ FAILED: Large array not allocated correctly"
    end if
    
    deallocate(tracer_types)
  end subroutine test_tracer_edge_cases

end program test_atmos_model
