!
      module transfer_to_long_integers
!
      use m_precision
!
      implicit none
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
      integer(kind = kint), intent(in) :: n
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
      integer(kind = kint), intent(in) :: n1, n2
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
      integer(kind = kint), intent(in) :: n
      integer(kind = kint_gl), intent(inout) :: id_a(n)
      integer(kind = kint) :: copy_to_short_array(n)
!
!$omp parallel workshare
      copy_to_short_array(1:n) = id_a(1:n)
!$omp end parallel workshare
!
      end function copy_to_short_array
!
!-----------------------------------------------------------------------
!
      function copy_to_short_darray(n1, n2, id_da)
!
      integer(kind = kint), intent(in) :: n1, n2
      integer(kind = kint_gl), intent(inout) :: id_da(n1, n2)
      integer(kind = kint) :: copy_to_short_darray(n1,n2)
!
!
!$omp parallel workshare
      copy_to_short_darray(1:n1,1:n2) = id_da(1:n1,1:n2)
!$omp end parallel workshare
!
      end function copy_to_short_darray
!
!-----------------------------------------------------------------------
!
      end module transfer_to_long_integers
