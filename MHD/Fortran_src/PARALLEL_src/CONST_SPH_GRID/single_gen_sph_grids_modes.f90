!>@file   single_gen_sph_grids_modes.f90
!!@brief  module single_gen_sph_grids_modes
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Set global spherical harmonics indices in local array
!!        (Serial version)
!!
!!@verbatim
!!      subroutine gen_sph_rlm_grids                                    &
!!     &         (ndomain_sph, sph_params, sph_rlm, comm_rlm_mul)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_comm_tbl), intent(inout) :: comm_rlm_mul(ndomain_sph)
!!      subroutine gen_sph_rtm_grids                                    &
!!     &         (ndomain_sph, sph_params, sph_rtm, comm_rtm_mul)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_comm_tbl), intent(inout) :: comm_rtm_mul(ndomain_sph)
!!
!!      subroutine gen_sph_rj_modes(ndomain_sph, comm_rlm_mul,          &
!!     &          sph_params, sph_rlm, sph_rj)
!!        type(sph_comm_tbl), intent(in) :: comm_rlm_mul(ndomain_sph)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rlm_grid), intent(inout) :: sph_rlm
!!        type(sph_rj_grid), intent(inout) :: sph_rj
!!      subroutine gen_sph_rtp_grids(ndomain_sph, comm_rtm_mul,         &
!!     &         sph_params, sph_rtp, sph_rtm)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_comm_tbl), intent(in) :: comm_rtm_mul(ndomain_sph)
!!        type(sph_rtm_grid), intent(inout) :: sph_rtm
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!
!!      subroutine gen_fem_mesh_for_sph(ndomain_sph, sph_params,       &
!!     &          sph_rj, sph_rtp)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_rtp_grid), intent(inout) :: sph_rtp
!!
!!      subroutine dealloc_all_comm_stacks_rlm(ndomain_sph, comm_rlm_mul)
!!      subroutine dealloc_all_comm_stacks_rtm(ndomain_sph, comm_rtm_mul)
!!@endverbatim
!
      module single_gen_sph_grids_modes
!
      use m_precision
      use m_machine_parameter
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
      use t_group_data
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gen_sph_rlm_grids                                      &
     &         (ndomain_sph, sph_params, sph_rlm, comm_rlm_mul)
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_comm_tbl), intent(inout) :: comm_rlm_mul(ndomain_sph)
!
      integer(kind = kint) :: ip_rank, ip
!
      type(sph_comm_tbl) :: comm_rlm_lc
!
!
      do ip = 1, ndomain_sph
        ip_rank = ip - 1
        call const_sph_rlm_modes(ip_rank, sph_rlm, comm_rlm_lc)
        call copy_sph_comm_neib(comm_rlm_lc, comm_rlm_mul(ip))
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &          'output_modes_rlm_sph_trans', ip_rank
        call output_modes_rlm_sph_trans                                 &
     &     (ip_rank, sph_params%l_truncation, sph_rlm, comm_rlm_lc)
!
        write(*,'(a,i6,a)') 'Legendre transform table rlm',             &
     &          ip_rank, ' is done.'
      end do
      write(*,*)
!
      end subroutine gen_sph_rlm_grids
!
! ----------------------------------------------------------------------
!
      subroutine gen_sph_rtm_grids                                      &
     &         (ndomain_sph, sph_params, sph_rtm, comm_rtm_mul)
!
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_comm_tbl), intent(inout) :: comm_rtm_mul(ndomain_sph)
!
      integer(kind = kint) :: ip_rank, ip
!
      type(sph_comm_tbl) :: comm_rtm_lc
!
!
      do ip = 1, ndomain_sph
        ip_rank = ip - 1
        call const_sph_rtm_grids(ip_rank, sph_rtm, comm_rtm_lc)
        call copy_sph_comm_neib(comm_rtm_lc, comm_rtm_mul(ip))
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &          'output_geom_rtm_sph_trans', ip_rank
        call output_geom_rtm_sph_trans                                  &
     &     (ip_rank, sph_params%l_truncation, sph_rtm, comm_rtm_lc)
!
        write(*,'(a,i6,a)') 'Legendre transform table rtm',             &
     &          ip_rank, ' is done.'
      end do
      write(*,*)
!
      end subroutine gen_sph_rtm_grids
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine gen_sph_rj_modes(ndomain_sph, comm_rlm_mul,            &
     &          sph_params, sph_rlm, sph_rj)
!
      use set_local_index_table_sph
      use set_comm_table_rtp_rj
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rlm_mul(ndomain_sph)
      type(sph_shell_parameters), intent(in) :: sph_params
!
      type(sph_rlm_grid), intent(inout) :: sph_rlm
      type(sph_rj_grid), intent(inout) :: sph_rj
!
      integer(kind = kint) :: ip_rank
!
!
      call allocate_rj_1d_local_idx(sph_rj)
      do ip_rank = 0, ndomain_sph-1
        call const_sph_rj_modes(ip_rank, ndomain_sph,                   &
     &      comm_rlm_mul, sph_params, sph_rj, sph_rlm)
      end do
      call deallocate_rj_1d_local_idx
!
      end subroutine gen_sph_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine gen_sph_rtp_grids(ndomain_sph, comm_rtm_mul,           &
     &         sph_params, sph_rtp, sph_rtm)
!
      use set_local_index_table_sph
      use set_comm_table_rtp_rj
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_comm_tbl), intent(in) :: comm_rtm_mul(ndomain_sph)
!
      type(sph_rtm_grid), intent(inout) :: sph_rtm
      type(sph_rtp_grid), intent(inout) :: sph_rtp
      integer(kind = kint) :: ip_rank
!
!
      call allocate_rtp_1d_local_idx(sph_rtp)
      do ip_rank = 0, ndomain_sph-1
        call const_sph_rtp_grids(ip_rank, ndomain_sph, comm_rtm_mul,    &
     &      sph_params, sph_rtp, sph_rtm)
      end do
      call deallocate_rtp_1d_local_idx
!
      end subroutine gen_sph_rtp_grids
!
! ----------------------------------------------------------------------
!
      subroutine gen_fem_mesh_for_sph(ndomain_sph, sph_params,         &
     &          sph_rj, sph_rtp)
!
      use m_gauss_points
      use m_sph_mesh_1d_connect
      use const_1d_ele_connect_4_sph
      use gen_sph_grids_modes
      use set_local_index_table_sph
      use set_sph_groups
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(sph_rtp_grid), intent(inout) :: sph_rtp
!
      integer(kind = kint) :: ip_rank
      type(group_data) :: radial_rj_grp_lc
!
!
      if(iflag_output_mesh .eq. 0) return
!
      call allocate_gauss_points(sph_rtp%nidx_global_rtp(2))
      call allocate_gauss_colatitude
      call construct_gauss_coefs
      call set_gauss_colatitude
!
      call s_const_1d_ele_connect_4_sph                                 &
     &   (sph_params%iflag_shell_mode, sph_params%m_folding, sph_rtp)
      call set_rj_radial_grp(sph_params, sph_rj, radial_rj_grp_lc)
!
      do ip_rank = 0, ndomain_sph-1
        call const_fem_mesh_for_sph                                     &
     &     (ip_rank, sph_params, radial_rj_grp_lc, sph_rtp)
      end do
!
      call deallocate_grp_type(radial_rj_grp_lc)
      call deallocate_gauss_points
      call deallocate_gauss_colatitude
!
      end subroutine gen_fem_mesh_for_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_all_comm_stacks_rlm(ndomain_sph, comm_rlm_mul)
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(inout) :: comm_rlm_mul(ndomain_sph)
      integer(kind = kint) :: ip
!
!
      do ip = 1, ndomain_sph
        call dealloc_type_sph_comm_stack(comm_rlm_mul(ip))
          comm_rlm_mul(ip)%nneib_domain = 0
      end do
!
      end subroutine dealloc_all_comm_stacks_rlm
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_all_comm_stacks_rtm(ndomain_sph, comm_rtm_mul)
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(inout) :: comm_rtm_mul(ndomain_sph)
      integer(kind = kint) :: ip
!
!
      do ip = 1, ndomain_sph
        call dealloc_type_sph_comm_stack(comm_rtm_mul(ip))
        comm_rtm_mul(ip)%nneib_domain = 0
      end do
!
      end subroutine dealloc_all_comm_stacks_rtm
!
! ----------------------------------------------------------------------
!
      end module single_gen_sph_grids_modes
