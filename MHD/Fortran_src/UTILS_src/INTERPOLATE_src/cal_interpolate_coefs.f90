!>@file   cal_interpolate_coefs.f90
!!@brief  module cal_interpolate_coefs
!!
!!@date  Programmed by H.Matsui in Aug. 2011
!
!>@brief find point in one element and return local coordinate
!!
!!@verbatim
!!      subroutine s_cal_interpolate_coefs                              &
!!     &         (x_target, gen_itp_p, org_node, org_ele,               &
!!     &          my_rank_org, inod, jele, error_level, iflag_message,  &
!!     &          itp_ele_work, iflag_org_domain, itp_coef_dest)
!!        type(ctl_params_4_gen_table), intent(in) :: gen_itp_p
!!        type(node_data), intent(in) :: new_node
!!        type(node_data), intent(in) :: org_node
!!        type(element_data), intent(in) :: org_ele
!!        type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
!!        type(cal_interpolate_coefs_work), intent(inout) :: itp_ele_work
!!
!!      subroutine set_results_2_array(id_rank_org, inod, jele, xi,     &
!!     &                               iflag_org_domain, itp_coef_dest)
!!        type(ctl_params_4_gen_table), intent(in) :: gen_itp_p
!!        type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
!!      subroutine set_results_2_array_fin(id_rank_org, inod, jele, xi, &
!!     &          differ_tmp, differ_res, iflag_org_tmp, itp_coef_dest)
!!        type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
!!
!!      subroutine check_interpolation                                  &
!!     &         (nnod_dest, internod_dest, xx_dest,                    &
!!     &          org_node, org_ele, itp_coef_dest,                     &
!!     &          id_file, my_rank_org, iflag_org_domain, itp_ele_work)
!!        type(node_data), intent(in) :: org_node
!!        type(element_data), intent(in) :: org_ele
!!        type(interpolate_coefs_dest), intent(in) :: itp_coef_dest
!!        type(cal_interpolate_coefs_work), intent(inout) :: itp_ele_work
!!@endverbatim
!
      module cal_interpolate_coefs
!
      use m_precision
      use t_find_interpolate_in_ele
!
      implicit none
!
      private :: set_results_2_array, set_results_2_array_fin
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine s_cal_interpolate_coefs                                &
     &         (x_target, gen_itp_p, org_node, org_ele,                 &
     &          my_rank_org, inod, jele, error_level, iflag_message,    &
     &          itp_ele_work, iflag_org_domain, itp_coef_dest)
!
      use calypso_mpi
      use t_ctl_params_4_gen_table
      use m_connect_hexa_2_tetra
      use subroutines_4_search_table
      use cal_local_position_by_tetra
      use modify_local_positions
      use solver_33_array
!
      use t_geometry_data
      use t_interpolate_coefs_dest
!
      real(kind=kreal), intent(in) :: x_target(3)
      type(ctl_params_4_gen_table), intent(in) :: gen_itp_p
!
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
!
      integer, intent(in) :: my_rank_org
      integer(kind = kint), intent(in) :: inod, jele
      integer(kind = kint), intent(in) :: iflag_message
      real(kind = kreal), intent(in) :: error_level
!
      integer(kind = kint), intent(inout) :: iflag_org_domain
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
      type(cal_interpolate_coefs_work), intent(inout) :: itp_ele_work
!
      real(kind=kreal) :: xi(3)
!
      integer (kind = kint) :: ierr_inter
!
      if (iflag_message .eq. 1) then
        write(my_rank+60,*) my_rank_org, inod, x_target(1:3)
      end if
!
      call find_interpolate_in_ele                                      &
     &   (x_target, gen_itp_p%maxitr, gen_itp_p%eps_iter,               &
     &    my_rank, iflag_message, error_level, org_node, org_ele,       &
     &    jele, itp_ele_work, xi, ierr_inter)
!
      if(ierr_inter.gt.0 .and. ierr_inter.le.gen_itp_p%maxitr) then
        call set_results_2_array(my_rank_org, inod, jele, xi,           &
     &                           iflag_org_domain, itp_coef_dest)
      else
        call set_results_2_array_fin(my_rank_org, inod, jele, xi,       &
     &                               itp_ele_work, itp_coef_dest)
      end if
!
      end subroutine s_cal_interpolate_coefs
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine set_results_2_array(id_rank_org, inod, jele, xi,       &
     &                               iflag_org_domain, itp_coef_dest)
!
      use t_ctl_params_4_gen_table
      use t_interpolate_coefs_dest
!
      integer, intent(in) :: id_rank_org
      integer(kind = kint), intent(in) :: inod, jele
      real(kind = kreal), intent(in) :: xi(3)
!
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
      integer(kind = kint), intent(inout) :: iflag_org_domain
!
      iflag_org_domain = id_rank_org + 1
      itp_coef_dest%iele_org_4_dest(inod) =  jele
      itp_coef_dest%coef_inter_dest(inod,1:3) = xi(1:3)
!
      end subroutine set_results_2_array
!
!-----------------------------------------------------------------------
!
      subroutine set_results_2_array_fin(id_rank_org, inod, jele, xi,   &
     &                                   itp_ele_work, itp_coef_dest)
!
      use m_constants
      use t_interpolate_coefs_dest
!
      integer, intent(in) :: id_rank_org
      integer(kind = kint), intent(in) :: inod, jele
      real(kind = kreal), intent(in) :: xi(3)
!
      type(interpolate_coefs_dest), intent(inout) :: itp_coef_dest
      type(cal_interpolate_coefs_work), intent(inout) :: itp_ele_work
!
!
      itp_ele_work%iflag_org_tmp = id_rank_org + 1
      itp_coef_dest%iele_org_4_dest(inod) =  jele
      itp_coef_dest%coef_inter_dest(inod,1:3) = xi(1:3)
      itp_ele_work%differ_tmp = itp_ele_work%differ_res
!
      end subroutine set_results_2_array_fin
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine check_interpolation                                    &
     &         (nnod_dest, internod_dest, xx_dest,                      &
     &          org_node, org_ele, itp_coef_dest,                       &
     &          id_file, my_rank_org, iflag_org_domain, itp_ele_work)
!
      use subroutines_4_search_table
      use cal_position_and_grad
!
      use t_geometry_data
      use t_interpolate_coefs_dest
!
      integer(kind = kint), intent(in) :: nnod_dest, internod_dest
      real(kind = kreal), intent(in) :: xx_dest(nnod_dest,3)
! 
      type(node_data), intent(in) :: org_node
      type(element_data), intent(in) :: org_ele
      type(interpolate_coefs_dest), intent(in) :: itp_coef_dest
      integer(kind = kint), intent(in)                                  &
     &                    :: iflag_org_domain(nnod_dest)
!
      integer, intent(in) :: my_rank_org
      integer(kind = kint), intent(in) :: id_file
      type(cal_interpolate_coefs_work), intent(inout) :: itp_ele_work
!
      integer(kind = kint) :: inod
      real(kind=kreal) :: xx_z(3), xi(3), diff(3)
      real(kind=kreal) :: dnxi(3), dnei(3), dnzi(3)
!
      write(id_file,*) '#'
      write(id_file,*) '#  node, difference from original node'
      write(id_file,*) '#'
!
      do inod = 1, internod_dest
        if(iflag_org_domain(inod) .eq. my_rank_org) then
!
          xi(1:3) = itp_coef_dest%coef_inter_dest(inod,1:3)
          call copy_position_2_2nd_local_ele(org_node, org_ele,         &
     &        itp_coef_dest%iele_org_4_dest(inod),                      &
     &        itp_ele_work%x_local_ele)
!
          call cal_position_and_gradient(org_ele%nnod_4_ele, xx_z,      &
     &        dnxi, dnei, dnzi, itp_ele_work%x_local_ele, xi)
!
          diff(1:3) = xx_z(1:3) - xx_dest(inod,1:3)
!
          write(id_file,*) inod, diff(1:3)
!
        end if
      end do
!
      end subroutine check_interpolation
!
!-----------------------------------------------------------------------
!
      end module cal_interpolate_coefs
