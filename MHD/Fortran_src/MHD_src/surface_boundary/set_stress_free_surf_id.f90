!
!     module set_stress_free_surf_id
!
!      Written by H. Matsui on Sep. 2005
!      Modified by H. Matsui on Feb. 2009
!
!      subroutine count_num_stress_free_surf(num_surf, surf_name,       &
!     &          num_bc_tq, bc_tq_name, ibc_tq_type,                    &
!     &          ngrp_sf_fr_in, ngrp_sf_fr_out)
!      subroutine s_stress_free_surf_id(num_surf, surf_name,            &
!     &          num_bc_tq, bc_tq_name, ibc_tq_type,                    &
!     &          ngrp_sf_fr_in, ngrp_sf_fr_out,                         &
!     &          id_grp_sf_fr_in, id_grp_sf_fr_out)
!
      module set_stress_free_surf_id
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
      subroutine count_num_stress_free_surf(num_surf, surf_name,        &
     &          num_bc_tq, bc_tq_name, ibc_tq_type,                     &
     &          ngrp_sf_fr_in, ngrp_sf_fr_out)
!
      integer(kind=kint), intent(in) :: num_surf
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint) :: num_bc_tq
      integer (kind=kint), intent(in) :: ibc_tq_type(num_bc_tq)
      character (len=kchara), intent(in) :: bc_tq_name(num_bc_tq)
!
      integer(kind=kint), intent(inout) :: ngrp_sf_fr_in
      integer(kind=kint), intent(inout) :: ngrp_sf_fr_out
!
      integer(kind = kint) :: i, j
!
!
      do i=1, num_surf
! ----------- loop for boundary conditions
        do j=1, num_bc_tq
!
! ----------- check surface group
          if (surf_name(i) .eq. bc_tq_name(j)) then
!
! -----------set boundary from control file
            if ( ibc_tq_type(j) .eq. (101) ) then
              ngrp_sf_fr_in = ngrp_sf_fr_in + 1
!
! -----------set boundary from data file
            else if ( ibc_tq_type(j) .eq. (102) ) then
              ngrp_sf_fr_out = ngrp_sf_fr_out + 1
            end if
          end if
!
        end do
      end do
!
      end subroutine count_num_stress_free_surf
!
!-----------------------------------------------------------------------
!
      subroutine s_stress_free_surf_id(num_surf, surf_name,             &
     &          num_bc_tq, bc_tq_name, ibc_tq_type,                     &
     &          ngrp_sf_fr_in, ngrp_sf_fr_out,                          &
     &          id_grp_sf_fr_in, id_grp_sf_fr_out)
!
      integer(kind=kint), intent(in) :: num_surf
      character(len=kchara), intent(in) :: surf_name(num_surf)
!
      integer (kind=kint) :: num_bc_tq
      integer (kind=kint), intent(in) :: ibc_tq_type(num_bc_tq)
      character (len=kchara), intent(in) :: bc_tq_name(num_bc_tq)
      integer(kind=kint), intent(in) :: ngrp_sf_fr_in, ngrp_sf_fr_out
!
      integer(kind=kint), intent(inout)                                 &
     &      :: id_grp_sf_fr_in(ngrp_sf_fr_in)
      integer(kind=kint), intent(inout)                                 &
     &      :: id_grp_sf_fr_out(ngrp_sf_fr_out)
!
      integer(kind = kint) :: i, j
      integer(kind = kint) :: l_f1, l_f2
!
!
      l_f1 = 0
      l_f2 = 0
!
      do i=1, num_surf
!
! ----------- loop for boundary conditions
        do j=1, num_bc_tq
!
! ----------- check surface group
          if (surf_name(i)==bc_tq_name(j)) then
!
! -----------set boundary from control file
            if ( ibc_tq_type(j)==(101) ) then
              l_f1 = l_f1 + 1
              id_grp_sf_fr_in(l_f1) = i
!
! -----------set boundary from data file
            else if ( ibc_tq_type(j)==(102) ) then
              l_f2 = l_f2 + 1
              id_grp_sf_fr_out(l_f2) = i
!
            end if
          end if
!
        end do
      end do
!
      end subroutine s_stress_free_surf_id
!
!-----------------------------------------------------------------------
!
      end module set_stress_free_surf_id
