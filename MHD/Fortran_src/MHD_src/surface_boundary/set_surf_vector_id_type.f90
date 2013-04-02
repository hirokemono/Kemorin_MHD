!
!      module set_surf_vector_id_type
!        (module set_surf_vector_id)
!        (module set_surf_scalar_id)
!
!      Written by H. Matsui on Feb., 2009
!
!      subroutine s_set_surf_vector_id_type                             &
!     &          (num_surf, surf_name, inod_stack_sf_grp,               &
!     &           num_bc_sf, bc_sf_name, ibc_sf_type,                   &
!     &           nmax_sf_sgs, id_grp_sf_sgs, ngrp_sf_fix_n,            &
!     &           id_grp_sf_fix_n, ist_nod_sf_fix_n)
!      subroutine set_sf_grad_vector_id_type                            &
!     &          (num_surf, surf_istack, surf_name,                     &
!     &           num_bc_sf, bc_sf_name, ibc_sf_type,                   &
!     &           nmax_sf_fix, id_grp_sf_fix,                           &
!     &           ist_ele_sf_fix, nmax_sf_lead, id_grp_sf_lead)
!
!      subroutine set_sf_grad_scalar_id_type                            &
!     &          (num_surf, surf_istack, surf_name,                     &
!     &           num_bc_sf, bc_sf_name, ibc_sf_type,                   &
!     &           ngrp_sf_fix, id_grp_sf_fix, ist_sf_fix,               &
!     &           ngrp_sf_lead, id_grp_sf_lead)
!
      module set_surf_vector_id_type
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
      subroutine s_set_surf_vector_id_type                              &
     &          (num_surf, surf_name, inod_stack_sf_grp,                &
     &           num_bc_sf, bc_sf_name, ibc_sf_type,                    &
     &           nmax_sf_sgs, id_grp_sf_sgs, ngrp_sf_fix_n,             &
     &           id_grp_sf_fix_n, ist_nod_sf_fix_n)
!
      integer(kind = kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: inod_stack_sf_grp(0:num_surf)
      character (len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint) :: num_bc_sf
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
!
      integer(kind=kint), intent(in) :: nmax_sf_sgs
      integer(kind=kint), intent(inout) :: id_grp_sf_sgs(nmax_sf_sgs,3)
!
!
      integer(kind=kint), intent(in) :: ngrp_sf_fix_n
      integer(kind=kint), intent(inout)                                 &
     &      :: id_grp_sf_fix_n(ngrp_sf_fix_n)
      integer(kind=kint), intent(inout)                                 &
     &      :: ist_nod_sf_fix_n(0:ngrp_sf_fix_n)
!
!
      integer(kind = kint) :: i, j, nd
      integer(kind = kint) :: l_s1(3), isig_s(3)
      integer(kind = kint) :: l_10
!
!
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
            if ( abs(ibc_sf_type(j)) .eq. 10) then
              l_10 = l_10 + 1
              id_grp_sf_fix_n(l_10) = i
!
              ist_nod_sf_fix_n(l_10) = ist_nod_sf_fix_n(l_10-1)         &
     &                 + inod_stack_sf_grp(i) - inod_stack_sf_grp(i-1)
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
      end subroutine s_set_surf_vector_id_type
!
!-----------------------------------------------------------------------
!
      subroutine set_sf_grad_vector_id_type                             &
     &          (num_surf, surf_istack, surf_name,                      &
     &           num_bc_sf, bc_sf_name, ibc_sf_type,                    &
     &           nmax_sf_fix, id_grp_sf_fix,                            &
     &           ist_ele_sf_fix, nmax_sf_lead, id_grp_sf_lead)
!
      integer(kind = kint), intent(in) :: num_surf
      integer(kind = kint), intent(in) :: surf_istack(0:num_surf)
      character (len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint) :: num_bc_sf
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
! 
      integer(kind=kint), intent(in) :: nmax_sf_fix
      integer(kind=kint), intent(inout) :: id_grp_sf_fix(nmax_sf_fix,3)
      integer(kind=kint), intent(inout)                                 &
     &      :: ist_ele_sf_fix(0:nmax_sf_fix,3)
!
      integer(kind=kint), intent(in) :: nmax_sf_lead
      integer(kind=kint), intent(inout) :: id_grp_sf_lead(nmax_sf_lead,3)
!
!
      integer(kind = kint) :: i, j, nd, i_dest
      integer(kind = kint) :: l_f1(3), l_l1(3)
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
! -----------set fixed boundary
!
            do nd = 1, 3
              if ( abs( ibc_sf_type(j) ) .eq. nd ) then
                l_f1(nd) = l_f1(nd) + 1
                i_dest = l_f1(nd)
                id_grp_sf_fix(i_dest,nd) = i
!
                ist_ele_sf_fix(i_dest,nd) = ist_ele_sf_fix(i_dest-1,nd) &
     &                    + surf_istack(i) - surf_istack(i-1)
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
      end subroutine set_sf_grad_vector_id_type
!
!-----------------------------------------------------------------------
!
      subroutine set_sf_grad_scalar_id_type                             &
     &          (num_surf, surf_istack, surf_name,                      &
     &           num_bc_sf, bc_sf_name, ibc_sf_type,                    &
     &           ngrp_sf_fix, id_grp_sf_fix, ist_sf_fix,                &
     &           ngrp_sf_lead, id_grp_sf_lead)
!
      integer (kind=kint), intent(in) :: num_surf
      integer (kind=kint), intent(in) :: surf_istack(0:num_surf)
      character (len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint), intent(in) :: num_bc_sf
      integer (kind=kint), intent(in) :: ibc_sf_type(num_bc_sf)
      character (len=kchara), intent(in) :: bc_sf_name(num_bc_sf)
!
      integer(kind=kint), intent(in) :: ngrp_sf_fix
      integer(kind=kint), intent(inout) :: id_grp_sf_fix(ngrp_sf_fix)
      integer(kind=kint), intent(inout) :: ist_sf_fix(0:ngrp_sf_fix)
! 
      integer(kind=kint), intent(in) :: ngrp_sf_lead
      integer(kind=kint), intent(inout) :: id_grp_sf_lead(ngrp_sf_lead)
!
      integer(kind = kint) :: l_f1, l_l1
      integer(kind = kint) :: i, j
!
!
      l_f1 = 0
      l_l1 = 0
!
! ---------  boundary condition for temperature
!
      do i=1, num_surf
        do j=1, num_bc_sf
          if (surf_name(i)==bc_sf_name(j)) then
!
! -----------set boundary using SGS case
!
            if (ibc_sf_type(j) .eq. 0 .or. ibc_sf_type(j) .eq. 1) then
              l_f1 = l_f1 + 1
              id_grp_sf_fix(l_f1) = i
              ist_sf_fix(l_f1) = ist_sf_fix(l_f1-1)                     &
     &                          + surf_istack(i) - surf_istack(i-1)
!
            else if (ibc_sf_type(j) .eq. 100) then
              l_l1 = l_l1 + 1
              id_grp_sf_lead(l_l1) = i
            end if
!
          end if
        end do
      end do
!
      end subroutine set_sf_grad_scalar_id_type
!
!-----------------------------------------------------------------------
!
      end module set_surf_vector_id_type
