program main

call add()

endprogram main

subroutine add()
    implicit none
    integer :: i, j, rows, cols
    real,allocatable :: matrixA(:,:), matrixB(:,:), result(:,:)
    character :: response

    print *, "Enter the number of rows:"
    read  *, rows
    print *, "Enter the number of coloumns:"
    read  *, cols
    
    allocate(matrixA(rows,cols), matrixB(rows,cols), result(rows,cols))
    
    
    print *, "Entre the elements of first matrix:"
    do i = 1, rows
        read *, (matrixA(i,j),j=1,cols)
    enddo
    
100  print *, "Entre the elements of next matrix:"
    do i = 1, rows
        read *, (matrixB(i,j),j=1,cols)
    enddo

    do i = 1, rows
        do j = 1, cols
            result(i,j) = 0.0
        enddo
    enddo
    
    do i = 1, rows
        do j = 1, cols
            result(i,j) = matrixA(i,j) + matrixB(i,j)
        enddo
    enddo
    
    print *, "The result is:"
    do i = 1, rows
        print *, (result(i,j), j=1,cols)
    enddo 

    print *, "Do you want to add one more matrix ? (Y/n)"
    read  *, response

    ! Convert response to uppercase for consistency
    response = achar(iachar(response) - merge(32, 0, (iachar(response) >= iachar('a') .and. iachar(response) <= iachar('z'))))

    if (response == 'Y') then
        matrixA(:,:) = result(:,:)
        goto 100
    else
        print *, "Thank You! "
    end if

endsubroutine add
