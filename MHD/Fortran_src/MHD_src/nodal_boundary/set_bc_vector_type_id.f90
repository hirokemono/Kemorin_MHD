!
!      module set_bc_vector_type_id
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by Kemorin on Jan. 2004
!        modified by Kemorin on Oct. 2005
!        modified by Kemorin on Jan., 2009
!
!      subroutine set_fixed_vector_type_id(num_bc_field, bc_field_name, &
!     &          ibc_field_type, bc_field_mag, nod_grp,                 &
!     &          vect_bc, field_name, ii)
!        integer (kind=kint), intent(in) :: num_bc_field
!        real (kind=kreal), intent(in) :: bc_field_mag(num_bc_field)
!        integer (kind=kint), intent(in) :: ibc_field_type(num_bc_field)
!        character(len=kchara), intent(in) :: bc_field_name(num_bc_field)
!        character(len=kchara), intent(in) :: field_name(3)
!        type(group_data),          intent(in) :: nod_grp
!        integer (kind = kint), intent(inout) :: ii(3)
!        type(vect_fixed_nod_bc_type), intent(inout) :: vect_bc
!      subroutine s_set_bc_vector_type_id(num_bc_field, bc_field_name,  &
!     &          ibc_field_type, bc_field_mag, nod_grp, vect_bc,        &
!     &          iref, ii)
!        integer (kind=kint), intent(in) :: iref
!        integer (kind=kint), intent(in) :: num_bc_field
!        real (kind=kreal), intent(in) :: bc_field_mag(num_bc_field)
!        integer (kind=kint), intent(in) :: ibc_field_type(num_bc_field)
!        character(len=kchara), intent(in) :: bc_field_name(num_bc_field)
!        type(group_data), intent(in) :: nod_grp
!        integer (kind = kint), intent(inout) :: ii(3)
!        type(vect_fixed_nod_bc_type), intent(inout) :: vect_bc
!      subroutine set_bc_rotate_type_id(num_bc_field, bc_field_name,    &
!     &          ibc_field_type, bc_field_mag, nod_grp, rotation,       &
!     &          iref, ii)
!        integer (kind=kint), intent(in) :: iref
!        integer (kind=kint), intent(in) :: num_bc_field
!        real (kind=kreal), intent(in) :: bc_field_mag(num_bc_field)
!        integer (kind=kint), intent(in) :: ibc_field_type(num_bc_field)
!        character (len=kchara), intent(in) :: bc_field_name(num_bc_fiel)
!        type(group_data), intent(in) :: nod_grp
!        integer (kind = kint), intent(inout) :: ii(3)
!        type(scaler_rotaion_nod_bc_type), intent(inout) :: rotation
!      subroutine set_sph_magne_type_id(num_bc_field, bc_field_name,    &
!     &          ibc_field_type, nod_grp, magne, l_f)
!        integer (kind=kint), intent(in) :: num_bc_field
!        integer (kind=kint), intent(in) :: ibc_field_type(num_bc_field)
!        character(len=kchara), intent(in) :: bc_field_name(num_bc_field)
!        type(group_data),          intent(in) :: nod_grp
!        integer (kind = kint), intent(inout) :: l_f(3)
!        type(vect_fixed_nod_bc_type), intent(inout) :: magne
!
      module set_bc_vector_type_id
!
      use m_precision
!
      use t_group_data
      use t_nodal_bc_data
      use set_nodal_bc_type
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_fixed_vector_type_id(num_bc_field, bc_field_name,  &
     &          ibc_field_type, bc_field_mag, nod_grp,                  &
     &          vect_bc, field_name, ii)
!
      use m_boundary_field_IO
!
      integer (kind=kint), intent(in) :: num_bc_field
      real (kind=kreal), intent(in) :: bc_field_mag(num_bc_field)
      integer (kind=kint), intent(in) :: ibc_field_type(num_bc_field)
      character (len=kchara), intent(in) :: bc_field_name(num_bc_field)
      character(len=kchara), intent(in) :: field_name(3)
!
      type(group_data),          intent(in) :: nod_grp
!
      integer (kind = kint), intent(inout) :: ii(3)
      type(vect_fixed_nod_bc_type), intent(inout) :: vect_bc
!
      integer (kind = kint) :: i, j, nd
!
!
      do i=1, nod_grp%num_grp
!
! ----------- loop for boundary conditions
        do j=1, num_bc_field 
!
! ----------- check node group
!
          if (nod_grp%grp_name(i) .eq. bc_field_name(j)) then
            do nd = 1, 3
!
! -----------set boundary from control file
!
              if (ibc_field_type(j)== nd) then
                call set_vect_nod_bc_type_from_ctl(ii, i, nd, nod_grp,  &
     &              vect_bc, bc_field_mag(j) )
!
! -----------set boundary from data file
!
              else if (ibc_field_type(j)==-nd) then
                call set_vect_nod_bc_type_from_data(ii, i, nd, nod_grp, &
     &              vect_bc, field_name(nd))
!
              end if
            end do
!
          end if
        end do
      end do
!
      end subroutine set_fixed_vector_type_id
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_bc_vector_type_id(num_bc_field, bc_field_name,   &
     &          ibc_field_type, bc_field_mag, nod_grp, vect_bc,         &
     &          iref, ii)
!
      integer (kind=kint), intent(in) :: iref
!
      integer (kind=kint), intent(in) :: num_bc_field
      real (kind=kreal), intent(in) :: bc_field_mag(num_bc_field)
      integer (kind=kint), intent(in) :: ibc_field_type(num_bc_field)
      character (len=kchara), intent(in) :: bc_field_name(num_bc_field)
!
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint), intent(inout) :: ii(3)
      type(vect_fixed_nod_bc_type), intent(inout) :: vect_bc
!
      integer (kind = kint) :: i, j, nd
!
!
      do i=1, nod_grp%num_grp
!
! ----------- loop for boundary conditions
!
        do j=1, num_bc_field 
!
! ----------- check node group
!
          if (nod_grp%grp_name(i) .eq. bc_field_name(j)) then
!
            do nd = 1, 3
              if (ibc_field_type(j) == (iref+nd) ) then
                call set_vect_nod_bc_type_from_ctl(ii, i, nd, nod_grp,  &
     &              vect_bc, bc_field_mag(j) )
              end if
            end do
!
          end if
        end do
      end do
!
      end subroutine s_set_bc_vector_type_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_rotate_type_id(num_bc_field, bc_field_name,     &
     &          ibc_field_type, bc_field_mag, nod_grp, rotation,        &
     &          iref, ii)
!
      integer (kind=kint), intent(in) :: iref
!
      integer (kind=kint), intent(in) :: num_bc_field
      real (kind=kreal), intent(in) :: bc_field_mag(num_bc_field)
      integer (kind=kint), intent(in) :: ibc_field_type(num_bc_field)
      character (len=kchara), intent(in) :: bc_field_name(num_bc_field)
!
      type(group_data), intent(in) :: nod_grp
!
      integer (kind = kint), intent(inout) :: ii(3)
      type(scaler_rotaion_nod_bc_type), intent(inout) :: rotation
!
      integer (kind = kint) :: i, j, nd
!
!
      do i=1, nod_grp%num_grp
!
        do j=1, num_bc_field 
!
         if (nod_grp%grp_name(i) .eq. bc_field_name(j)) then
          do nd = 1, 3
            if ( ibc_field_type(j) == (nd+iref) ) then
              call set_rot_nod_bc_type_from_ctl(ii, i, nd, nod_grp,     &
     &            rotation, bc_field_mag(j) )
            end if
          end do
!
         end if
        end do
      end do
!
      end subroutine set_bc_rotate_type_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_sph_magne_type_id(num_bc_field, bc_field_name,     &
     &          ibc_field_type, nod_grp, magne, l_f)
!
      use m_boundary_field_IO
!
      integer (kind=kint), intent(in) :: num_bc_field
      integer (kind=kint), intent(in) :: ibc_field_type(num_bc_field)
      character (len=kchara), intent(in) :: bc_field_name(num_bc_field)
      type(group_data),          intent(in) :: nod_grp
!
      integer (kind = kint), intent(inout) :: l_f(3)
      type(vect_fixed_nod_bc_type), intent(inout) :: magne
!
      integer (kind = kint) :: i, j
!
!
      do i=1, nod_grp%num_grp 
        do j=1, num_bc_field
!
          if ( nod_grp%grp_name(i) .eq. bc_field_name(j)) then
            if ( ibc_field_type(j) .eq.  999 ) then
              call set_magne_nod_bc_type_from_sph(l_f, i, nod_grp,      &
     &            magne )
            end if
          end if
!
        end do
      end do
!
      end subroutine set_sph_magne_type_id
!
!  ---------------------------------------------------------------------
!
      end module set_bc_vector_type_id
