!
!      module set_sf_grad_vector_id
!
!      Written by H. Matsui on Sep. 2005
!
!      subroutine count_num_sf_grad_vector                              &
!     &          (num_surf, surf_istack, surf_name, num_bc_sf,          &
!     &           bc_sf_name, ibc_sf_type, bc_sf_mag,                   &
!     &           field_name_x, field_name_y, field_name_z,             &
!     &           nmax_sf_fix, nmax_ele_sf_fix, nmax_sf_lead,           &
!     &           ngrp_sf_fix, nele_sf_fix, ngrp_sf_lead)
!      subroutine s_set_sf_grad_vector_id                               &
!     &          (num_surf, surf_istack, surf_name,                     &
!     &           num_bc_sf, bc_sf_name, ibc_sf_type, bc_sf_mag,        &
!     &           field_name_x, field_name_y, field_name_z,             &
!     &           nmax_sf_fix, id_grp_sf_fix,                           &
!     &           nmax_ele_sf_fix, ist_ele_sf_fix, sf_apt_fix,          &
!     &           nmax_sf_lead, id_grp_sf_lead)
!
      module set_sf_grad_vector_id
!
      use set_surface_bc
!
      use m_precision
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine count_num_sf_grad_vector                               &
     &          (num_surf, surf_istack, surf_name, num_bc_sf,           &
     &           bc_sf_name, ibc_sf_type, bc_sf_mag,                    &
     &           field_name_x, field_name_y, field_name_z,              &
     &           nmax_sf_fix, nmax_ele_sf_fix, nmax_sf_lead,            &
     &           ngrp_sf_fix, nele_sf_fix, ngrp_sf_lead)
!
      integer(kind=kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint) :: num_bc_sf
      real (kind=kreal), intent(in) :: bc_sf_mag(num_bc_sf)
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
      character(len=kchara), intent(in) :: field_name_x
      character(len=kchara), intent(in) :: field_name_y
      character(len=kchara), intent(in) :: field_name_z
! 
! 
      integer(kind=kint), intent(inout) :: nmax_sf_fix, nmax_ele_sf_fix
      integer(kind=kint), intent(inout) :: nmax_sf_lead
      integer(kind=kint), intent(inout) :: ngrp_sf_fix(3)
      integer(kind=kint), intent(inout) :: nele_sf_fix(3)
      integer(kind=kint), intent(inout) :: ngrp_sf_lead(3)
!
      integer(kind = kint) :: i, j, nd
      character(len=kchara) :: field_name
!
!
      nmax_sf_fix =       0
      nmax_ele_sf_fix =   0
      nmax_sf_lead =      0
      ngrp_sf_fix(1:3) =  0
      nele_sf_fix(1:3) =  0
      ngrp_sf_lead(1:3) = 0
!
      do i=1, num_surf
        if (num_bc_sf .gt. 0) then
!
! ----------- loop for boundary conditions
          do j=1, num_bc_sf
!
! ----------- check surface group
            if (surf_name(i)==bc_sf_name(j)) then
!
! -----------set boundary from control file
              do nd = 1, 3
                if ( ibc_sf_type(j)==(nd) ) then
                  ngrp_sf_fix(nd) = ngrp_sf_fix(nd) + 1
                  nele_sf_fix(nd) = nele_sf_fix(nd)                     &
     &                         + surf_istack(i) - surf_istack(i-1)
!
! -----------set boundary from data file
                else if ( ibc_sf_type(j)==(-nd) ) then
                  if      (nd .eq. 1) then
                    field_name = field_name_x
                  else if (nd .eq. 2) then
                    field_name = field_name_y
                  else if (nd .eq. 3) then
                    field_name = field_name_z
                  end if
!
                  call count_surf_group_from_data(i, ngrp_sf_fix(nd),   &
     &                nele_sf_fix(nd), field_name,                      &
     &                num_surf, surf_istack, surf_name )
!
! -----------set boundary from data file
!
                else if ( ibc_sf_type(j)==(nd+100) ) then
                  ngrp_sf_lead(nd) = ngrp_sf_lead(nd) + 1
                end if
!
              end do
            end if
!
          end do
        end if
      end do
!
      do j = 1, 3
        nmax_sf_fix = max(nmax_sf_fix,ngrp_sf_fix(j))
        nmax_ele_sf_fix = max(nmax_ele_sf_fix,nele_sf_fix(j))
        nmax_sf_lead = max(nmax_sf_lead,ngrp_sf_lead(j))
      end do
!
      end subroutine count_num_sf_grad_vector
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_sf_grad_vector_id                                &
     &          (num_surf, surf_istack, surf_name,                      &
     &           num_bc_sf, bc_sf_name, ibc_sf_type, bc_sf_mag,         &
     &           field_name_x, field_name_y, field_name_z,              &
     &           nmax_sf_fix, id_grp_sf_fix,                            &
     &           nmax_ele_sf_fix, ist_ele_sf_fix, sf_apt_fix,           &
     &           nmax_sf_lead, id_grp_sf_lead)
!
      integer(kind = kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
      character (len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint) :: num_bc_sf
      real (kind=kreal), intent(in) :: bc_sf_mag(num_bc_sf)
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
      character(len=kchara), intent(in) :: field_name_x
      character(len=kchara), intent(in) :: field_name_y
      character(len=kchara), intent(in) :: field_name_z
! 
      integer(kind=kint), intent(in) :: nmax_sf_fix, nmax_ele_sf_fix
      integer(kind=kint), intent(inout) :: id_grp_sf_fix(nmax_sf_fix,3)
      integer(kind=kint), intent(inout)                                 &
     &      :: ist_ele_sf_fix(0:nmax_sf_fix,3)
      real (kind=kreal), intent(inout) :: sf_apt_fix(nmax_ele_sf_fix,3)
!
      integer(kind=kint), intent(in) :: nmax_sf_lead
      integer(kind=kint), intent(inout) :: id_grp_sf_lead(nmax_sf_lead,3)
!
!
      integer(kind = kint) :: i, j, nd
      integer(kind = kint) :: l_f1(3), l_l1(3)
      character (len=kchara) :: field_name
!
!
      l_f1(1:3) = 0
      l_l1(1:3) = 0
!
      do i=1, num_surf
!
! ----------- loop for boundary conditions
        do j=1, num_bc_sf
!
! ----------- check surface group
          if (surf_name(i)==bc_sf_name(j)) then
!
! -----------set boundary from control file
!
            do nd = 1, 3
              if ( ibc_sf_type(j)==(nd) ) then
                call set_surf_group_from_ctl(num_surf, surf_istack,     &
                    nmax_sf_fix, nmax_ele_sf_fix, l_f1(nd), i,          &
     &              id_grp_sf_fix(1,nd), ist_ele_sf_fix(0,nd),          &
     &              sf_apt_fix(1,nd), bc_sf_mag(j))
!
! -----------set boundary from data file
              else if ( ibc_sf_type(j)==(-nd) ) then
                if      (nd .eq. 1) then
                  field_name = field_name_x
                else if (nd .eq. 2) then
                  field_name = field_name_y
                else if (nd .eq. 3) then
                  field_name = field_name_z
                end if
!
                call set_surf_group_from_data(num_surf, surf_name,      &
     &              nmax_sf_fix, nmax_ele_sf_fix, l_f1(nd), i,          &
     &              id_grp_sf_fix(1,nd), ist_ele_sf_fix(0,nd),          &
     &              sf_apt_fix(1,nd), field_name )
!
! -----------lead boundary values
              else if ( ibc_sf_type(j)==(nd+100) ) then
                l_l1(nd) = l_l1(nd) + 1
                id_grp_sf_lead(l_l1(nd),nd) = i
              end if
            end do
!
          end if
!
        end do
      end do
!
      end subroutine s_set_sf_grad_vector_id
!
!-----------------------------------------------------------------------
!
      end module set_sf_grad_vector_id
