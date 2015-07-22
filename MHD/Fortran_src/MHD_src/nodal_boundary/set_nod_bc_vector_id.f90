!set_nod_bc_vector_id.f90
!      module set_nod_bc_vector_id
!
!        programmed by H.Matsui and H.Okuda
!                                    on July 2000 (ver 1.1)
!        modified by Kemorin on Jan. 2004
!        modified by Kemorin on Oct. 2005
!
!!      subroutine set_fixed_vector_id                                  &
!!     &         (nod_grp, num_bc_field, bc_field_name,                 &
!!     &          ibc_field_type, bc_field_mag, ibc, ibc2, nmax_bc_nod, &
!!     &          ibc_id, bc_apt, field_name, ii)
!!      subroutine set_bc_vector_id                                     &
!!     &         (nod_grp, num_bc_field, bc_field_name,                 &
!!     &          ibc_field_type, bc_field_mag, ibc, ibc2, nmax_bc_nod, &
!!     &          ibc_id, bc_apt, iref, ii)
!!      subroutine set_bc_rotate_id(nod_grp, num_bc_field,              &
!!     &          bc_field_name, ibc_field_type, bc_field_mag,          &
!!     &          ibc, ibc2, num_bc_nod, ibc_id, bc_apt, iref, ii)
!!      subroutine set_sph_magne_id(nod_grp, num_bc_field,              &
!!     &          bc_field_name, ibc_field_type, l_f)
!
      module set_nod_bc_vector_id
!
      use m_precision
!
      use m_geometry_parameter
      use t_group_data
      use set_nodal_boundary
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_fixed_vector_id                                    &
     &         (nod_grp, num_bc_field, bc_field_name,                   &
     &          ibc_field_type, bc_field_mag, ibc, ibc2, nmax_bc_nod,   &
     &          ibc_id, bc_apt, field_name, ii)
!
      type(group_data), intent(in) :: nod_grp
      integer (kind=kint), intent(in) :: num_bc_field
      real (kind=kreal), intent(in) :: bc_field_mag(num_bc_field)
      integer (kind=kint), intent(in) :: ibc_field_type(num_bc_field)
      character (len=kchara), intent(in) :: bc_field_name(num_bc_field)
      character(len=kchara), intent(in) :: field_name(3)
!
      integer (kind=kint), intent(inout) :: ibc(numnod,3)
      integer (kind=kint), intent(inout) :: ibc2(numnod,3)
!
      integer (kind=kint), intent(in) :: nmax_bc_nod
      integer (kind=kint), intent(inout) :: ibc_id(nmax_bc_nod,3)
      real (kind=kreal),   intent(inout) :: bc_apt(nmax_bc_nod,3)
!
      integer (kind = kint), intent(inout) :: ii(3)
      integer (kind = kint) :: i, j, nd
!
!
      do i=1, nod_grp%num_grp 
!
! ----------- loop for boundary conditions
        do j=1, num_bc_field 
!
! ----------- check node group
          if (nod_grp%grp_name(i) .eq. bc_field_name(j)) then
            do nd = 1, 3
!
! -----------set boundary from control file
              if (ibc_field_type(j)== nd) then
                call set_nod_bc_from_ctl(nod_grp, numnod, nmax_bc_nod,  &
     &              ii(nd), i, ibc_id(1,nd), ibc(1,nd), ibc2(1,nd),     &
     &              bc_apt(1,nd), bc_field_mag(j) )
!
! -----------set boundary from data file
              else if (ibc_field_type(j)==-nd) then
                call set_nod_bc_from_data(nod_grp, numnod, nmax_bc_nod, &
     &              ii(nd), i, ibc_id(1,nd), ibc(1,nd), ibc2(1,nd),     &
     &              bc_apt(1,nd), field_name(nd))
              end if
            end do
!
          end if
        end do
      end do
!
      end subroutine set_fixed_vector_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_vector_id                                       &
     &         (nod_grp, num_bc_field, bc_field_name,                   &
     &          ibc_field_type, bc_field_mag, ibc, ibc2, nmax_bc_nod,   &
     &          ibc_id, bc_apt, iref, ii)
!
      integer (kind=kint), intent(in) :: iref
!
      type(group_data), intent(in) :: nod_grp
      integer (kind=kint), intent(in) :: num_bc_field
      real (kind=kreal), intent(in) :: bc_field_mag(num_bc_field)
      integer (kind=kint), intent(in) :: ibc_field_type(num_bc_field)
      character (len=kchara), intent(in) :: bc_field_name(num_bc_field)
!
      integer (kind=kint), intent(inout) :: ibc(numnod,3)
      integer (kind=kint), intent(inout) :: ibc2(numnod,3)
!
      integer (kind=kint), intent(in) :: nmax_bc_nod
      integer (kind=kint), intent(inout) :: ibc_id(nmax_bc_nod,3)
      real (kind=kreal),   intent(inout) :: bc_apt(nmax_bc_nod,3)
!
      integer (kind = kint), intent(inout) :: ii(3)
      integer (kind = kint) :: i, j, nd
!
!
      do i=1, nod_grp%num_grp 
!
! ----------- loop for boundary conditions
        do j=1, num_bc_field 
!
! ----------- check node group
          if (nod_grp%grp_name(i) .eq. bc_field_name(j)) then
            do nd = 1, 3
!
              if (ibc_field_type(j) .eq. (iref+nd) ) then
                call set_nod_bc_from_ctl(nod_grp, numnod, nmax_bc_nod,  &
     &            ii(nd), i, ibc_id(1,nd), ibc(1,nd), ibc2(1,nd),       &
     &            bc_apt(1,nd), bc_field_mag(j) )
              end if
            end do
!
          end if
        end do
      end do
!
      end subroutine set_bc_vector_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_bc_rotate_id(nod_grp, num_bc_field,                &
     &          bc_field_name, ibc_field_type, bc_field_mag,            &
     &          ibc, ibc2, num_bc_nod, ibc_id, bc_apt, iref, ii)
!
      use m_geometry_parameter
!
      integer (kind=kint), intent(in) :: iref
!
      type(group_data), intent(in) :: nod_grp
      integer (kind=kint), intent(in) :: num_bc_field
      real (kind=kreal), intent(in) :: bc_field_mag(num_bc_field)
      integer (kind=kint), intent(in) :: ibc_field_type(num_bc_field)
      character (len=kchara), intent(in) :: bc_field_name(num_bc_field)
!
      integer (kind=kint), intent(inout) :: ibc(numnod)
      integer (kind=kint), intent(inout) :: ibc2(numnod)
!
      integer (kind=kint), intent(in) :: num_bc_nod
      integer (kind=kint), intent(inout) :: ibc_id(num_bc_nod)
      real (kind=kreal),   intent(inout) :: bc_apt(num_bc_nod,3)
!
      integer (kind = kint), intent(inout) :: ii(3)
      integer (kind = kint) :: i, j, nd
!
!
      do i=1, nod_grp%num_grp 
        do j=1, num_bc_field 
!
          if (nod_grp%grp_name(i) .eq. bc_field_name(j)) then
            do nd = 1, 3
              if ( ibc_field_type(j) == (nd+iref) ) then
                call set_nod_bc_from_ctl(nod_grp, numnod, num_bc_nod,   &
     &            ii(nd), i, ibc_id, ibc, ibc2, bc_apt(1,nd),           &
     &            bc_field_mag(j) )
              end if
            end do
!
          end if
        end do
      end do
!
      end subroutine set_bc_rotate_id
!
!  ---------------------------------------------------------------------
!
      subroutine set_sph_magne_id(nod_grp, num_bc_field,                &
     &          bc_field_name, ibc_field_type, l_f)
!
      use set_radial_magne_sph
!
      type(group_data), intent(in) :: nod_grp
      integer (kind=kint), intent(in) :: num_bc_field
      integer (kind=kint), intent(in) :: ibc_field_type(num_bc_field)
      character (len=kchara), intent(in) :: bc_field_name(num_bc_field)
!
      integer (kind = kint), intent(inout) :: l_f(3)
      integer (kind = kint) :: i, j
!
!
      do i=1, nod_grp%num_grp 
        do j=1, num_bc_field 
!
          if (nod_grp%grp_name(i) .eq. bc_field_name(j)) then
!
            if ( ibc_field_type(j) == 999 ) then
              call set_r_magne_sph(nod_grp, l_f, i, j)
            end if
!
          end if
        end do
      end do
!
      end subroutine set_sph_magne_id
!
!  ---------------------------------------------------------------------
!
      end module set_nod_bc_vector_id
