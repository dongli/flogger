module log_mod

  use hash_table_mod
  use string_mod
  use face

  implicit none

  private

  public log_init
  public log_add_diag
  public log_notice
  public log_warning
  public log_error
  public log_print_diag

  type(hash_table_type) diags

contains

  subroutine log_init()

    diags = hash_table()

    call log_notice('Log module is initialized.')

  end subroutine log_init

  subroutine log_add_diag(name, value)

    character(*), intent(in) :: name
    class(*), intent(in) :: value

    call diags%insert(name, value)

    select type (value)
    type is (integer)
    type is (real(4))
    type is (real(8))
    class default
      call log_error('Unsupported diagnostic value type!')
    end select

  end subroutine log_add_diag

  subroutine log_notice(message, file, line)

    character(*), intent(in) :: message
    character(*), intent(in), optional :: file
    integer, intent(in), optional :: line

    if (present(file) .and. present(line)) then
      write(6, *) '[' // colorize('Notice', color_fg='green') // ']: ' // &
                  trim(file) // ':' // trim(to_string(line)) // ': ' // trim(message)
    else
      write(6, *) '[Notice]: ' // trim(message)
    end if

  end subroutine log_notice

  subroutine log_warning(message, file, line)

    character(*), intent(in) :: message
    character(*), intent(in), optional :: file
    integer, intent(in), optional :: line

    if (present(file) .and. present(line)) then
      write(6, *) '[' // colorize('Warning', color_fg='yellow') // ']: ' // &
                  trim(file) // ':' // trim(to_string(line)) // ': ' // trim(message)
    else
      write(6, *) '[Warning]: ' // trim(message)
    end if

  end subroutine log_warning

  subroutine log_error(message, file, line)

    character(*), intent(in) :: message
    character(*), intent(in), optional :: file
    integer, intent(in), optional :: line

    if (present(file) .and. present(line)) then
      write(6, *) '[' // colorize('Error', color_fg='red') // ']: ' // &
                  trim(file) // ':' // trim(to_string(line)) // ': ' // trim(message)
    else
      write(6, *) '[' // colorize('Error', color_fg='red') // ']: ' // trim(message)
    end if
    stop 1

  end subroutine log_error

  subroutine log_print_diag(prefix)

    character(*), intent(in) :: prefix

    type(hash_table_iterator_type) iter

    write(6, '(A)', advance='no') colorize('==> ', color_fg='blue') // trim(prefix)

    iter = hash_table_iterator(diags)
    do while (.not. iter%ended())
      select type (value => iter%value)
      type is (integer)
        write(6, '(X, A)', advance='no') trim(to_string(value))
      type is (real(4))
        write(6, '(X, A)', advance='no') trim(to_string(value, 20))
      type is (real(8))
        write(6, '(X, A)', advance='no') trim(to_string(value, 20))
      class default
        write(6, '(X, A)', advance='no') iter%key
      end select
      call iter%next()
    end do
    write(6, *)

  end subroutine log_print_diag

end module log_mod
