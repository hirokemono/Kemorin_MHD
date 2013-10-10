!
!      module count_num_nodal_fields
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by Kemorin on Jan. 2004
!        modified by Kemorin on Oct. 2005
!        modified by Kemorin on Oct. 2009
!
!      subroutine count_num_bc_scalar(num_bc, bc_istack, bc_name,       &
!     &          num_bc_field,  bc_field_name, ibc_field_type,          &
!     &          num_bc_nod, iref)
!      subroutine count_num_bc_vector(num_bc, bc_istack, bc_name,       &
!     &          num_bc_field, bc_field_name, ibc_field_type,           &
!     &          num_bc_nod, iref)
!      subroutine add_num_bc_magne(num_bc, bc_istack, bc_name,          &
!     &          num_bc_field, bc_field_name, ibc_field_type,           &
!     &          num_bc_nod)
!      subroutine add_num_bc_mag_p(num_bc, bc_istack, bc_name,          &
!     &          num_bc_field, bc_field_name, ibc_field_type,           &
!     &          num_bc_nod)
!      subroutine cal_max_int_4_vector(nmax, num)
!
      module count_num_nodal_fields
!
      use m_precision
!
      implicit none
!
      private :: count_nod_bc
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_scalar(num_bc, bc_istack, bc_name,        &
     &          num_bc_field,  bc_field_name, ibc_field_type,           &
     &          num_bc_nod, iref)
!
      integer(kind=kint),    intent(in) :: num_bc
      integer(kind=kint),    intent(in) :: bc_istack(0:num_bc)
      character(len=kchara), intent(in) :: bc_name(num_bc)
      integer(kind=kint),    intent(in) :: iref
      integer(kind=kint),    intent(in) :: num_bc_field
      integer(kind=kint),    intent(in) :: ibc_field_type(num_bc_field)
      character(len=kchara), intent(in) :: bc_field_name(num_bc_field)
!
      integer (kind=kint), intent(inout) :: num_bc_nod
!
      integer (kind = kint) :: i, j
!
!
      num_bc_nod  = 0
!
      do i=1, num_bc
!
       if (num_bc_field /= 0) then
        do j=1, num_bc_field
         if (bc_name(i)==bc_field_name(j)) then
!
            if ( abs(ibc_field_type(j)) .eq. iref ) then
              call count_nod_bc(i, num_bc, bc_istack, num_bc_nod )
            end if
!
         end if
        end do
       end if
!
      end do
!
      end subroutine count_num_bc_scalar
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_bc_vector(num_bc, bc_istack, bc_name,        &
     &          num_bc_field, bc_field_name, ibc_field_type,            &
     &          num_bc_nod, iref)
!
      integer(kind=kint),    intent(in) :: num_bc
      integer(kind=kint),    intent(in) :: bc_istack(0:num_bc)
      character(len=kchara), intent(in) :: bc_name(num_bc)
      integer(kind=kint),    intent(in) :: iref
      integer(kind=kint),    intent(in) :: num_bc_field
      integer(kind=kint),    intent(in) :: ibc_field_type(num_bc_field)
      character(len=kchara), intent(in) :: bc_field_name(num_bc_field)
!
      integer(kind=kint), intent(inout) :: num_bc_nod(3)
!
      integer(kind = kint) :: i, j, nd
!
!
      num_bc_nod(1:3)  = 0
!
      do i=1, num_bc
!
       if (num_bc_field /= 0) then
        do j=1, num_bc_field
         if (bc_name(i)==bc_field_name(j)) then
!
          do nd = 1, 3
            if ( abs(ibc_field_type(j)) == (nd+iref) ) then
              call count_nod_bc(i, num_bc, bc_istack, num_bc_nod(nd) )
            end if
          end do
!
         end if
        end do
       end if
!
      end do
!
      end subroutine count_num_bc_vector
!
!  ---------------------------------------------------------------------
!
      subroutine add_num_bc_magne(num_bc, bc_istack, bc_name,           &
     &          num_bc_field, bc_field_name, ibc_field_type,            &
     &          num_bc_nod)
!
      integer (kind=kint),    intent(in) :: num_bc
      integer (kind=kint),    intent(in) :: bc_istack(0:num_bc)
      character (len=kchara), intent(in) :: bc_name(num_bc)
      integer (kind=kint),    intent(in) :: num_bc_field
      integer (kind=kint),    intent(in) :: ibc_field_type(num_bc_field)
      character (len=kchara), intent(in) :: bc_field_name(num_bc_field)
!
      integer (kind=kint), intent(inout) :: num_bc_nod(3)
!
      integer (kind = kint) :: i, j, nd
!
!
      do i=1, num_bc
!
       if (num_bc_field /= 0) then
        do j=1, num_bc_field
         if (bc_name(i)==bc_field_name(j)) then
!
          if ( abs(ibc_field_type(j)) == 999 ) then
            do nd = 1, 3
              call count_nod_bc(i, num_bc, bc_istack, num_bc_nod(nd) )
            end do
          end if
!
         end if
        end do
       end if
!
      end do
!
      end subroutine add_num_bc_magne
!
!  ---------------------------------------------------------------------
!
      subroutine add_num_bc_mag_p(num_bc, bc_istack, bc_name,           &
     &          num_bc_field, bc_field_name, ibc_field_type,            &
     &          num_bc_nod)
!
      integer (kind=kint),    intent(in) :: num_bc
      integer (kind=kint),    intent(in) :: bc_istack(0:num_bc)
      character (len=kchara), intent(in) :: bc_name(num_bc)
      integer (kind=kint),    intent(in) :: num_bc_field
      integer (kind=kint),    intent(in) :: ibc_field_type(num_bc_field)
      character (len=kchara), intent(in) :: bc_field_name(num_bc_field)
!
      integer (kind=kint), intent(inout) :: num_bc_nod
!
      integer (kind = kint) :: i, j
!
!
      do i=1, num_bc
!
       if (num_bc_field /= 0) then
        do j=1, num_bc_field
         if (bc_name(i)==bc_field_name(j)) then
!
          if ( abs(ibc_field_type(j)) == 999 ) then
            call count_nod_bc(i, num_bc, bc_istack, num_bc_nod )
          end if
!
         end if
        end do
       end if
!
      end do
!
      end subroutine add_num_bc_mag_p
!
!  ---------------------------------------------------------------------
!
      subroutine cal_max_int_4_vector(nmax, num)
!
      integer (kind=kint), intent(inout) :: nmax
      integer (kind=kint), intent(in) :: num(3)
!
      integer (kind = kint) :: nd
!
      nmax = 0
      do nd = 1, 3
        nmax = max( nmax, num(nd) )
      end do
!
      end subroutine cal_max_int_4_vector
!
!  ---------------------------------------------------------------------
!
      subroutine count_nod_bc(i, num_bc, bc_istack, num_bc_nod)
!
      integer(kind = kint), intent(in) :: i
      integer(kind = kint), intent(in) :: num_bc
      integer(kind = kint), intent(in) :: bc_istack(0:num_bc)
      integer(kind = kint), intent(inout) :: num_bc_nod
!
      num_bc_nod = num_bc_nod + bc_istack(i) - bc_istack(i-1)
!
      end subroutine count_nod_bc
!
!  ---------------------------------------------------------------------
!
      end module count_num_nodal_fields
