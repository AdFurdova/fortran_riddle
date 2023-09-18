program main
  implicit none
  integer :: key
  
  key = print_riddle()
  read *, key
  call decrypt(key)

contains
  integer function print_riddle()
    implicit none
    character(len=180) :: multiline

    multiline = "RIDDLE:" // new_line('a') // &
    "Three days on the road, we go," // new_line('a') // &
    "From city lights to sunsets aglow." // new_line('a') // &
    "Each morning a new skyline to see," // new_line('a') // &
    "Tell me, what number could the crew be?" // new_line('a')

    print *, multiline
    print *, "Enter your answer to the riddle:"
    
  end function print_riddle

  subroutine decrypt(key)
    implicit none
    integer :: key, i, y, position, resultIndex
    character(61) :: alphabet
    character(len=100) :: encryptedValue
    character(1) :: currentChar, decodedChar

    alphabet = "qY9FpNiM2ajz0GoW4bldVeU5Qk7AhvmwKJLOHc6PnXRS8Bf1yrZ3xDTEuCtgs"
    
    print *, "Enter the text you want to be decrypted:"
    read *, encryptedValue

    do i = 1, len_trim(encryptedValue)
      currentChar = encryptedValue(i:i)

      do y = 1, len(alphabet)
        if (alphabet(y:y) == currentChar) then
          position = y-key
          ! Checking overflow
          position = position - len(alphabet) * &
                                floor(1.0_4 * (position - 1_4) / len(alphabet))
          exit
        end if
      end do

      decodedChar = alphabet(position:position)
      write(encryptedValue(i:i), '(A, 2X, A)') decodedChar
    end do

    print*, "Ad's answer is: ", encryptedValue
  end subroutine decrypt
end program main
