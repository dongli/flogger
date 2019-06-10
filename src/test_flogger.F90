program test_flogger

  use flogger

  call log_notice('This is a notice.')
  call log_warning('This is a warning.')
  call log_error('This is an error!')

end program test_flogger
