program test_output_hours
  use fv3atm_cap_mod, only: OutputHours_FrequencyInput, OutputHours_ArrayInput
  use module_fv3_config, only: dt_atmos, output_fh
  use module_fv3_io_def, only: lflname_fulltime
  
  implicit none
  
  ! Test variables
  integer :: test_passed, test_failed
  integer :: i, expected_size
  logical :: test_result
  
  ! Variables for testing
  real :: nfhmax, output_startfh, outputfh2(2)
  integer :: noutput_fh
  
  ! Initialize test counters
  test_passed = 0
  test_failed = 0
  
  ! Set dt_atmos (time step in seconds) - needed by the subroutines
  ! This would normally be set by the model configuration
  dt_atmos = 450.0  ! 7.5 minutes = 450 seconds
  
  ! Run all tests
  call test_frequency_input()
  call test_array_input()
  
  ! Print summary
  print *, '======================================'
  print *, 'Unit Test Summary for OutputHours:'
  print *, 'Tests Passed: ', test_passed
  print *, 'Tests Failed: ', test_failed
  if (test_failed == 0) then
    print *, 'Status: ALL TESTS PASSED'
    stop 0  ! Exit with success
  else
    print *, 'Status: SOME TESTS FAILED'
    stop 1  ! Exit with error code for CI/CD
  end if
  print *, '======================================'
  
contains

  !> Test OutputHours_FrequencyInput subroutine
  subroutine test_frequency_input()
    
    print *, ''
    print *, '=== Testing OutputHours_FrequencyInput ==='
    
    ! Test 1: Basic frequency input with start at 0
    print *, 'Test 1: Basic frequency (every 3 hours for 24 hours)'
    nfhmax = 24.0
    output_startfh = 0.0
    outputfh2(1) = 3.0   ! frequency of 3 hours
    outputfh2(2) = -1.0  ! indicates frequency mode
    lflname_fulltime = .false.
    
    if (allocated(output_fh)) deallocate(output_fh)
    call OutputHours_FrequencyInput(nfhmax, output_startfh, outputfh2)
    
    expected_size = 9  ! 0.125, 3, 6, 9, 12, 15, 18, 21, 24
    test_result = .true.
    
    if (size(output_fh) /= expected_size) then
      print *, '  FAILED: Expected size', expected_size, 'but got', size(output_fh)
      test_result = .false.
    else
      print *, '  Array size correct:', size(output_fh)
      print *, '  Output hours:', output_fh
      
      ! Check first value (should be dt_atmos/3600 = 0.125)
      if (abs(output_fh(1) - dt_atmos/3600.0) > 1e-6) then
        print *, '  FAILED: First value should be', dt_atmos/3600.0, 'but got', output_fh(1)
        test_result = .false.
      end if
      
      ! Check lflname_fulltime (should be true because 0.125 is not integer hour)
      if (.not. lflname_fulltime) then
        print *, '  FAILED: lflname_fulltime should be true for non-integer hours'
        test_result = .false.
      end if
    end if
    
    if (test_result) then
      print *, '  PASSED'
      test_passed = test_passed + 1
    else
      test_failed = test_failed + 1
    end if
    
    ! Test 2: Frequency input with non-zero start
    print *, ''
    print *, 'Test 2: Frequency with non-zero start (start=6, every 6 hours for 48 hours)'
    nfhmax = 48.0
    output_startfh = 6.0
    outputfh2(1) = 6.0   ! frequency of 6 hours
    outputfh2(2) = -1.0  ! indicates frequency mode
    lflname_fulltime = .false.
    
    if (allocated(output_fh)) deallocate(output_fh)
    call OutputHours_FrequencyInput(nfhmax, output_startfh, outputfh2)
    
    expected_size = 8  ! 6, 12, 18, 24, 30, 36, 42, 48
    test_result = .true.
    
    if (size(output_fh) /= expected_size) then
      print *, '  FAILED: Expected size', expected_size, 'but got', size(output_fh)
      test_result = .false.
    else
      print *, '  Array size correct:', size(output_fh)
      print *, '  Output hours:', output_fh
      
      ! Check first value (should be 6.0)
      if (abs(output_fh(1) - 6.0) > 1e-6) then
        print *, '  FAILED: First value should be 6.0 but got', output_fh(1)
        test_result = .false.
      end if
      
      ! Check lflname_fulltime (should be false for all integer hours)
      if (lflname_fulltime) then
        print *, '  FAILED: lflname_fulltime should be false for all integer hours'
        test_result = .false.
      end if
    end if
    
    if (test_result) then
      print *, '  PASSED'
      test_passed = test_passed + 1
    else
      test_failed = test_failed + 1
    end if
    
    ! Test 3: Frequency that creates non-integer hours
    print *, ''
    print *, 'Test 3: Frequency creating non-integer hours (every 2.5 hours)'
    nfhmax = 10.0
    output_startfh = 0.0
    outputfh2(1) = 2.5   ! frequency of 2.5 hours
    outputfh2(2) = -1.0  ! indicates frequency mode
    lflname_fulltime = .false.
    
    if (allocated(output_fh)) deallocate(output_fh)
    call OutputHours_FrequencyInput(nfhmax, output_startfh, outputfh2)
    
    test_result = .true.
    print *, '  Output hours:', output_fh
    
    ! Check lflname_fulltime (should be true because of non-integer hours like 2.5, 7.5)
    if (.not. lflname_fulltime) then
      print *, '  FAILED: lflname_fulltime should be true for non-integer hours'
      test_result = .false.
    else
      print *, '  lflname_fulltime correctly set to true'
    end if
    
    if (test_result) then
      print *, '  PASSED'
      test_passed = test_passed + 1
    else
      test_failed = test_failed + 1
    end if
    
    ! Test 4: Edge case - nfhmax equals output_startfh
    print *, ''
    print *, 'Test 4: Edge case - nfhmax equals output_startfh'
    nfhmax = 6.0
    output_startfh = 6.0
    outputfh2(1) = 3.0
    outputfh2(2) = -1.0
    
    if (allocated(output_fh)) deallocate(output_fh)
    call OutputHours_FrequencyInput(nfhmax, output_startfh, outputfh2)
    
    test_result = .true.
    if (allocated(output_fh)) then
      print *, '  FAILED: output_fh should not be allocated when nfhmax <= output_startfh'
      print *, '  Array size:', size(output_fh)
      test_result = .false.
    else
      print *, '  Correctly handles edge case (no allocation)'
    end if
    
    if (test_result) then
      print *, '  PASSED'
      test_passed = test_passed + 1
    else
      test_failed = test_failed + 1
    end if
    
  end subroutine test_frequency_input
  
  !> Test OutputHours_ArrayInput subroutine
  subroutine test_array_input()
    
    print *, ''
    print *, '=== Testing OutputHours_ArrayInput ==='
    
    ! Test 1: Basic array input with start at 0
    print *, 'Test 1: Basic array input [0, 3, 6, 9, 12]'
    noutput_fh = 5
    output_startfh = 0.0
    lflname_fulltime = .false.
    
    if (allocated(output_fh)) deallocate(output_fh)
    allocate(output_fh(noutput_fh))
    output_fh = (/ 0.0, 3.0, 6.0, 9.0, 12.0 /)
    
    call OutputHours_ArrayInput(noutput_fh, output_startfh)
    
    test_result = .true.
    print *, '  Output hours:', output_fh
    
    ! Check first value (should be dt_atmos/3600 = 0.125)
    if (abs(output_fh(1) - dt_atmos/3600.0) > 1e-6) then
      print *, '  FAILED: First value should be', dt_atmos/3600.0, 'but got', output_fh(1)
      test_result = .false.
    end if
    
    ! Check lflname_fulltime (should be true because first hour is 0.125)
    if (.not. lflname_fulltime) then
      print *, '  FAILED: lflname_fulltime should be true'
      test_result = .false.
    end if
    
    if (test_result) then
      print *, '  PASSED'
      test_passed = test_passed + 1
    else
      test_failed = test_failed + 1
    end if
    
    ! Test 2: Array input with non-zero start
    print *, ''
    print *, 'Test 2: Array with non-zero start (start=6, array=[0, 6, 12, 18])'
    noutput_fh = 4
    output_startfh = 6.0
    lflname_fulltime = .false.
    
    if (allocated(output_fh)) deallocate(output_fh)
    allocate(output_fh(noutput_fh))
    output_fh = (/ 0.0, 6.0, 12.0, 18.0 /)
    
    call OutputHours_ArrayInput(noutput_fh, output_startfh)
    
    test_result = .true.
    print *, '  Output hours:', output_fh
    
    ! Check values (should be shifted by start: 6, 12, 18, 24)
    if (abs(output_fh(1) - 6.0) > 1e-6 .or. &
        abs(output_fh(2) - 12.0) > 1e-6 .or. &
        abs(output_fh(3) - 18.0) > 1e-6 .or. &
        abs(output_fh(4) - 24.0) > 1e-6) then
      print *, '  FAILED: Values not correctly shifted'
      test_result = .false.
    end if
    
    ! Check lflname_fulltime (should be false for all integer hours)
    if (lflname_fulltime) then
      print *, '  FAILED: lflname_fulltime should be false'
      test_result = .false.
    end if
    
    if (test_result) then
      print *, '  PASSED'
      test_passed = test_passed + 1
    else
      test_failed = test_failed + 1
    end if
    
    ! Test 3: Array with non-integer hours
    print *, ''
    print *, 'Test 3: Array with non-integer hours [1.5, 3, 4.5, 6]'
    noutput_fh = 4
    output_startfh = 0.0
    lflname_fulltime = .false.
    
    if (allocated(output_fh)) deallocate(output_fh)
    allocate(output_fh(noutput_fh))
    output_fh = (/ 1.5, 3.0, 4.5, 6.0 /)
    
    call OutputHours_ArrayInput(noutput_fh, output_startfh)
    
    test_result = .true.
    print *, '  Output hours:', output_fh
    
    ! Check lflname_fulltime (should be true because of non-integer hours)
    if (.not. lflname_fulltime) then
      print *, '  FAILED: lflname_fulltime should be true for non-integer hours'
      test_result = .false.
    else
      print *, '  lflname_fulltime correctly set to true'
    end if
    
    if (test_result) then
      print *, '  PASSED'
      test_passed = test_passed + 1
    else
      test_failed = test_failed + 1
    end if
    
    ! Test 4: Array with fractional hours from non-zero start
    print *, ''
    print *, 'Test 4: Non-zero start creating fractional hours (start=0.5, array=[0, 3, 6])'
    noutput_fh = 3
    output_startfh = 0.5
    lflname_fulltime = .false.
    
    if (allocated(output_fh)) deallocate(output_fh)
    allocate(output_fh(noutput_fh))
    output_fh = (/ 0.0, 3.0, 6.0 /)
    
    call OutputHours_ArrayInput(noutput_fh, output_startfh)
    
    test_result = .true.
    print *, '  Output hours:', output_fh
    
    ! Check values (should be 0.5, 3.5, 6.5)
    if (abs(output_fh(1) - 0.5) > 1e-6 .or. &
        abs(output_fh(2) - 3.5) > 1e-6 .or. &
        abs(output_fh(3) - 6.5) > 1e-6) then
      print *, '  FAILED: Values not correctly shifted'
      test_result = .false.
    end if
    
    ! Check lflname_fulltime (should be true because all hours are fractional)
    if (.not. lflname_fulltime) then
      print *, '  FAILED: lflname_fulltime should be true for non-integer hours'
      test_result = .false.
    else
      print *, '  lflname_fulltime correctly set to true'
    end if
    
    if (test_result) then
      print *, '  PASSED'
      test_passed = test_passed + 1
    else
      test_failed = test_failed + 1
    end if
    
    ! Test 5: Array with first element non-zero but start=0
    print *, ''
    print *, 'Test 5: First element non-zero with start=0 (array=[3, 6, 9, 12])'
    noutput_fh = 4
    output_startfh = 0.0
    lflname_fulltime = .false.
    
    if (allocated(output_fh)) deallocate(output_fh)
    allocate(output_fh(noutput_fh))
    output_fh = (/ 3.0, 6.0, 9.0, 12.0 /)
    
    call OutputHours_ArrayInput(noutput_fh, output_startfh)
    
    test_result = .true.
    print *, '  Output hours:', output_fh
    
    ! Check that values remain unchanged (no special handling of first element)
    if (abs(output_fh(1) - 3.0) > 1e-6) then
      print *, '  FAILED: First value should remain 3.0 but got', output_fh(1)
      test_result = .false.
    end if
    
    ! Check lflname_fulltime (should be false since all are integer hours)
    if (lflname_fulltime) then
      print *, '  FAILED: lflname_fulltime should be false for all integer hours'
      test_result = .false.
    end if
    
    if (test_result) then
      print *, '  PASSED'
      test_passed = test_passed + 1
    else
      test_failed = test_failed + 1
    end if
    
  end subroutine test_array_input
  
end program test_output_hours
