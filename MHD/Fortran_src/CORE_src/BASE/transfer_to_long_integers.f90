!
      module transfer_to_long_integers
!
      use m_precision
!
      implicit none
!
      type tmp_i8_2darray
        integer(kind = kint_gl) :: n1
        integer(kind = kint_gl) :: n2
        integer(kind = kint_gl), allocatable :: id_da(:,:)
      end type tmp_i8_2darray
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      integer(kind = kint_gl) function cast_long(i_in)
!
      integer(kind = kint), intent(in) :: i_in
!
      cast_long = i_in
!
      end function cast_long
!
!-----------------------------------------------------------------------
!
      function dup_to_long_array(n, i4_in)
!
      integer(kind = kint_gl), intent(in) :: n
      integer(kind = kint), intent(in) :: i4_in(n)
      integer(kind = kint_gl) :: dup_to_long_array(n)
!
!$omp parallel workshare
      dup_to_long_array(1:n) = i4_in(1:n)
!$omp end parallel workshare
!
      end function dup_to_long_array
!
!-----------------------------------------------------------------------
!
      function dup_to_long_darray(n1, n2, i4_in)
!
      integer(kind = kint_gl), intent(in) :: n1, n2
      integer(kind = kint), intent(in) :: i4_in(n1,n2)
      integer(kind = kint_gl) :: dup_to_long_darray(n1,n2)
!
!$omp parallel workshare
      dup_to_long_darray(1:n1,1:n2) = i4_in(1:n1,1:n2)
!$omp end parallel workshare
!
      end function dup_to_long_darray
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      integer(kind = kint) function cast_short(i8_in)
!
      integer(kind = kint_gl) :: i8_in
!
      cast_short = i8_in
!
      end function cast_short
!
!-----------------------------------------------------------------------
!
      function copy_to_short_array(n, id_a)
!
      integer(kind = kint_gl), intent(in) :: n
      integer(kind = kint_gl), intent(in) :: id_a(n)
      integer(kind = kint) :: copy_to_short_array(n)
!
!$omp parallel workshare
      copy_to_short_array(1:n) = id_a(1:n)
!$omp end parallel workshare
!
      end function copy_to_short_array
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine alloc_2d_i8array(n1, n2, tmp)
!
      integer(kind = kint_gl), intent(in) :: n1, n2
      type(tmp_i8_2darray), intent(inout) :: tmp
!
!
      tmp%n1 = n1
      tmp%n2 = n2
      allocate(tmp%id_da(tmp%n1,tmp%n2))
!
      end subroutine alloc_2d_i8array
!
!-----------------------------------------------------------------------
!
      subroutine dup_to_short_darray(tmp, i4_out)
!
      type(tmp_i8_2darray), intent(inout) :: tmp
      integer(kind = kint), intent(inout) :: i4_out(tmp%n1,tmp%n2)
!
!
      if(tmp%n1*tmp%n2 .gt. 0) then
!$omp parallel workshare
        i4_out(1:tmp%n1,1:tmp%n2) = tmp%id_da(1:tmp%n1,1:tmp%n2)
!$omp end parallel workshare
      end if
!
      deallocate(tmp%id_da)
!
      end subroutine dup_to_short_darray
!
!-----------------------------------------------------------------------
!
      end module transfer_to_long_integers
