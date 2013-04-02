!
!      module set_bc_scalar_type_id
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by Kemorin on Jan. 2004
!        modified by Kemorin on Oct. 2005
!        modified by Kemorin on Jan., 2009
!
!      subroutine set_fixed_bc_scalar_type_id(num_bc_field,             &
!     &          bc_field_name, ibc_field_type, bc_field_mag,           &
!     &          field_name, nod_grp, scalar_bc, ii)
!        integer (kind=kint), intent(in) :: num_bc_field
!        real (kind=kreal), intent(in) :: bc_field_mag(num_bc_field)
!        integer (kind=kint), intent(in) :: ibc_field_type(num_bc_field)
!        character(len=kchara), intent(in) :: bc_field_name(num_bc_field)
!        character(len=kchara), intent(in) :: field_name
!        type(group_data),          intent(in) :: nod_grp
!        type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!        integer (kind = kint), intent(inout) :: ii
!      subroutine s_set_bc_scalar_type_id(num_bc_field, bc_field_name,  &
!     &          ibc_field_type, bc_field_mag, nod_grp, scalar_bc,      &
!     &          iref, ii)
!        integer (kind=kint), intent(in) :: iref
!        integer (kind=kint), intent(in) :: num_bc_field
!        real (kind=kreal), intent(in) :: bc_field_mag(num_bc_field)
!        integer (kind=kint), intent(in) :: ibc_field_type(num_bc_field)
!        character (len=kchara), intent(in) :: bc_field_name(num_bc_field)
!        type(group_data), intent(in) :: nod_grp
!        type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!        integer (kind = kint), intent(inout) :: ii
!      subroutine set_bc_sph_magp_type_id(num_bc_field, bc_field_name,  &
!     &          ibc_field_type, nod_grp, scalar_bc, ii)
!        integer (kind=kint), intent(in) :: num_bc_field
!        integer (kind=kint), intent(in) :: ibc_field_type(num_bc_field)
!        character (len=kchara), intent(in) :: bc_field_name(num_bc_field)
!        type(group_data),          intent(in) :: nod_grp
!        integer (kind = kint), intent(inout) :: ii
!        type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
      module set_bc_scalar_type_id
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
      subroutine set_fixed_bc_scalar_type_id(num_bc_field,              &
     &          bc_field_name, ibc_field_type, bc_field_mag,            &
     &          field_name, nod_grp, scalar_bc, ii)
!
      use m_boundary_field_IO
!
      integer (kind=kint), intent(in) :: num_bc_field
      real (kind=kreal), intent(in) :: bc_field_mag(num_bc_field)
      integer (kind=kint), intent(in) :: ibc_field_type(num_bc_field)
      character(len=kchara), intent(in) :: bc_field_name(num_bc_field)
      character(len=kchara), intent(in) :: field_name
! 
      type(group_data),          intent(in) :: nod_grp
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
      integer (kind = kint), intent(inout) :: ii
!
      integer (kind = kint) :: i, j
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
          if ( nod_grp%grp_name(i) .eq.  bc_field_name(j)) then
!
! -----------set boundary from control file
!
            if ( ibc_field_type(j) .eq. 1 ) then
              call set_nod_bc_type_from_ctl (ii, i, nod_grp,            &
     &           scalar_bc, bc_field_mag(j) )
!
! -----------set boundary from data file
!
            else if ( ibc_field_type(j).eq. -1 ) then
              call set_nod_bc_type_from_data(ii, i, nod_grp,            &
     &            scalar_bc, field_name)
            end if
!
          end if
        end do
      end do
!
      end subroutine set_fixed_bc_scalar_type_id
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_bc_scalar_type_id(num_bc_field, bc_field_name,   &
     &          ibc_field_type, bc_field_mag, nod_grp, scalar_bc,       &
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
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
      integer (kind = kint), intent(inout) :: ii
!
      integer (kind = kint) :: i, j
!
      ii = 0
!
      do i=1, nod_grp%num_grp 
        do j=1, num_bc_field
          if ( nod_grp%grp_name(i) .eq. bc_field_name(j)) then
!
            if ( ibc_field_type(j) .eq. iref) then
              call set_nod_bc_type_from_ctl (ii, i, nod_grp,            &
     &              scalar_bc, bc_field_mag(j) )
            end if
!
          end if
        end do
      end do
!
      end subroutine s_set_bc_scalar_type_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_sph_magp_type_id(num_bc_field, bc_field_name,   &
     &          ibc_field_type, nod_grp, scalar_bc, ii)
!
      use m_boundary_field_IO
!
      integer (kind=kint), intent(in) :: num_bc_field
      integer (kind=kint), intent(in) :: ibc_field_type(num_bc_field)
      character (len=kchara), intent(in) :: bc_field_name(num_bc_field)
      type(group_data),          intent(in) :: nod_grp
!
      integer (kind = kint), intent(inout) :: ii
      type(scaler_fixed_nod_bc_type), intent(inout) :: scalar_bc
!
      integer (kind = kint) :: i, j
!
!
      do i=1, nod_grp%num_grp  
        do j=1, num_bc_field
          if ( nod_grp%grp_name(i) .eq. bc_field_name(j)) then
!
            if ( ibc_field_type(j) .eq. 999 ) then
              call set_mag_p_nod_bc_type_from_sph(ii, i, nod_grp,       &
     &            scalar_bc )
            end if
!
          end if
        end do
      end do
!
      end subroutine set_bc_sph_magp_type_id
!
!  ---------------------------------------------------------------------
!
      end module set_bc_scalar_type_id
