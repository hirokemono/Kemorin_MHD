!
!      module set_surf_vector_id
!
!      Written by H. Matsui on Sep. 2005
!
!      subroutine s_count_num_surf_vector                               &
!     &          (num_surf, inod_stack_sf_grp, surf_name,               &
!     &           num_bc_sf, bc_sf_name, ibc_sf_type, bc_sf_mag,        &
!     &           field_name, nmax_sf_sgs, ngrp_sf_sgs, ngrp_sf_dat_n,  &
!     &           nnod_sf_dat_n)
!      subroutine s_set_surf_vector_id                                  &
!     &          (num_surf, surf_name, inod_stack_sf_grp,               &
!     &           num_bc_sf, bc_sf_name, ibc_sf_type, bc_sf_mag,        &
!     &           field_name, nmax_sf_sgs, id_grp_sf_sgs,               &
!     &           ngrp_sf_fix_n, id_grp_sf_fix_n,                       &
!     &           nnod_sf_fix_n, ist_nod_sf_fix_n, sf_apt_fix_n)
!
      module set_surf_vector_id
!
      use m_precision
!
      use set_surface_bc
!
      implicit  none
!
!-----------------------------------------------------------------------
!
      contains 
!
!-----------------------------------------------------------------------
!
      subroutine s_count_num_surf_vector                                &
     &          (num_surf, inod_stack_sf_grp, surf_name,                &
     &           num_bc_sf, bc_sf_name, ibc_sf_type, bc_sf_mag,         &
     &           field_name, nmax_sf_sgs, ngrp_sf_sgs, ngrp_sf_dat_n,   &
     &           nnod_sf_dat_n)
!
      integer(kind=kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint) :: num_bc_sf
      real (kind=kreal), intent(in) :: bc_sf_mag(num_bc_sf)
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
      character (len=kchara), intent(in) :: field_name
! 
! 
      integer(kind=kint), intent(inout) :: nmax_sf_sgs
      integer(kind=kint), intent(inout) :: ngrp_sf_sgs(3)
!
      integer(kind=kint), intent(inout) :: ngrp_sf_dat_n, nnod_sf_dat_n
!
      integer(kind = kint) :: isig_s(3)
      integer(kind = kint) :: i, j, nd
!
!
      nmax_sf_sgs = 0
      ngrp_sf_sgs(1:3) = 0
!
      ngrp_sf_dat_n = 0
      nnod_sf_dat_n = 0
!
      do i=1, num_surf
!
        if (num_bc_sf .gt. 0) then
!
! ----------- loop for boundary conditions
          do j=1, num_bc_sf
!
! ----------- check surface group
            if (surf_name(i)==bc_sf_name(j)) then
              isig_s(1:3) = 0
!
! -----------set boundary from control file
              do nd = 1, 3
!
! -----------set boundary from control file
                if ( ibc_sf_type(j)==(14+nd) ) then
                  isig_s(nd) = 1
                end if
              end do
!
! -----------set boundary from control file
              if (ibc_sf_type(j)==10) then
                ngrp_sf_dat_n = ngrp_sf_dat_n + 1
                nnod_sf_dat_n = nnod_sf_dat_n                           &
     &                  + inod_stack_sf_grp(i) - inod_stack_sf_grp(i-1)
                isig_s(1:3) = 1
              else if (ibc_sf_type(j)==-10) then
                call count_surf_nod_group_from_data(i, ngrp_sf_dat_n,   &
     &              nnod_sf_dat_n, field_name, num_surf,                &
     &              inod_stack_sf_grp, surf_name)
                isig_s(1:3) = 1
              end if
!
              ngrp_sf_sgs(1:3) = ngrp_sf_sgs(1:3) + isig_s(1:3)
!
            end if
!
          end do
        end if
      end do
!
      do j = 1, 3
        nmax_sf_sgs = max(nmax_sf_sgs,ngrp_sf_sgs(j))
      end do
!
      end subroutine s_count_num_surf_vector
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_surf_vector_id                                   &
     &          (num_surf, surf_name, inod_stack_sf_grp,                &
     &           num_bc_sf, bc_sf_name, ibc_sf_type, bc_sf_mag,         &
     &           field_name, nmax_sf_sgs, id_grp_sf_sgs,                &
     &           ngrp_sf_fix_n, id_grp_sf_fix_n,                        &
     &           nnod_sf_fix_n, ist_nod_sf_fix_n, sf_apt_fix_n)
!
      integer(kind = kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
      character (len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint) :: num_bc_sf
      real (kind=kreal), intent(in) :: bc_sf_mag(num_bc_sf)
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
      character (len=kchara), intent(in) :: field_name
!
      integer(kind=kint), intent(in) :: nmax_sf_sgs
      integer(kind=kint), intent(inout) :: id_grp_sf_sgs(nmax_sf_sgs,3)
!
!
      integer(kind=kint), intent(in) :: ngrp_sf_fix_n
      integer(kind=kint), intent(in) :: nnod_sf_fix_n
      integer(kind=kint), intent(inout)                                 &
     &      :: id_grp_sf_fix_n(ngrp_sf_fix_n)
      integer(kind=kint), intent(inout)                                 &
     &      :: ist_nod_sf_fix_n(0:ngrp_sf_fix_n)
      real (kind=kreal), intent(inout) :: sf_apt_fix_n(nnod_sf_fix_n,3)
!
!
      integer(kind = kint) :: i, j, nd
      integer(kind = kint) :: l_f1(3), l_s1(3), isig_s(3)
      integer(kind = kint) :: l_10
!
!
      l_f1(1:3) = 0
      l_s1(1:3) = 0
      l_10 = 0
!
      do i=1, num_surf
!
! ----------- loop for boundary conditions
        do j=1, num_bc_sf
!
! ----------- check surface group
         if (surf_name(i)==bc_sf_name(j)) then
           isig_s(1:3) = 0
!
! -----------set boundary from control file
!
           do nd = 1, 3
!
! -----------set boundary from control file
              if ( ibc_sf_type(j)==(14+nd) ) then
                isig_s(nd) = 1
              end if
            end do
!
! -----------set boundary from control file
!
            if (ibc_sf_type(j)==10) then
              call set_sf_nod_grp_from_ctl(num_surf, inod_stack_sf_grp, &
     &            ngrp_sf_fix_n, nnod_sf_fix_n, l_10, i,                &
     &            id_grp_sf_fix_n, ist_nod_sf_fix_n, sf_apt_fix_n,      &
     &            bc_sf_mag(j))
              isig_s(1:3) = 1
            else if (ibc_sf_type(j)==-10) then
              call set_sf_nod_grp_from_data(num_surf, surf_name,        &
     &            ngrp_sf_fix_n, nnod_sf_fix_n, l_10, i,                &
     &            id_grp_sf_fix_n, ist_nod_sf_fix_n, sf_apt_fix_n,      &
     &            field_name )
              isig_s(1:3) = 1
            end if
!
            do nd = 1, 3
              if ( isig_s(nd).eq.1 ) then
                l_s1(nd) = l_s1(nd) + 1
                id_grp_sf_sgs(l_s1(nd),nd) = i
              end if
            end do
!
          end if
!
        end do
      end do
!
      end subroutine s_set_surf_vector_id
!
!-----------------------------------------------------------------------
!
      end module set_surf_vector_id
