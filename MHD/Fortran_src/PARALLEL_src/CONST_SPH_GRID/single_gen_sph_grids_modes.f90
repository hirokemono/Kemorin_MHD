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
!!      subroutine gen_sph_rlm_grids(ndomain_sph, comm_rlm)
!!      subroutine gen_sph_rtm_grids(ndomain_sph, comm_rtm)
!!
!!      subroutine gen_sph_rj_modes(ndomain_sph, comm_rlm)
!!      subroutine gen_sph_rtp_grids(ndomain_sph, comm_rtm)
!!
!!      subroutine gen_fem_mesh_for_sph(ndomain_sph)
!!
!!      subroutine dealloc_all_comm_stacks_rlm(ndomain_sph, comm_rlm)
!!      subroutine dealloc_all_comm_stacks_rtm(ndomain_sph, comm_rtm)
!!@endverbatim
!
      module single_gen_sph_grids_modes
!
      use m_precision
      use m_machine_parameter
      use t_sph_trans_comm_tbl
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine gen_sph_rlm_grids(ndomain_sph, comm_rlm)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(inout) :: comm_rlm(ndomain_sph)
      integer(kind = kint) :: ip_rank, ip
!
!
      do ip = 1, ndomain_sph
        ip_rank = ip - 1
        call const_sph_rlm_modes(ip_rank)
        call copy_sph_comm_neib(comm_rlm1, comm_rlm(ip))
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &          'output_modes_rlm_sph_trans', ip_rank
        call output_modes_rlm_sph_trans                                 &
     &     (ip_rank, l_truncation, sph_rlm1)
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
      subroutine gen_sph_rtm_grids(ndomain_sph, comm_rtm)
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use set_comm_table_rtp_rj
      use load_data_for_sph_IO
      use gen_sph_grids_modes
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(inout) :: comm_rtm(ndomain_sph)
      integer(kind = kint) :: ip_rank, ip
!
!
      do ip = 1, ndomain_sph
        ip_rank = ip - 1
        call const_sph_rtm_grids(ip_rank)
        call copy_sph_comm_neib(comm_rtm1, comm_rtm(ip))
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &          'output_geom_rtm_sph_trans', ip_rank
        call output_geom_rtm_sph_trans(ip_rank, l_truncation, sph_rtm1)
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
      subroutine gen_sph_rj_modes(ndomain_sph, comm_rlm)
!
      use set_local_index_table_sph
      use set_comm_table_rtp_rj
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rlm(ndomain_sph)
      integer(kind = kint) :: ip_rank
!
!
      call allocate_rj_1d_local_idx
      do ip_rank = 0, ndomain_sph-1
        call const_sph_rj_modes(ip_rank, ndomain_sph, comm_rlm)
      end do
      call deallocate_rj_1d_local_idx
!
      end subroutine gen_sph_rj_modes
!
! ----------------------------------------------------------------------
!
      subroutine gen_sph_rtp_grids(ndomain_sph, comm_rtm)
!
      use set_local_index_table_sph
      use set_comm_table_rtp_rj
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(in) :: comm_rtm(ndomain_sph)
      integer(kind = kint) :: ip_rank
!
!
      call allocate_rtp_1d_local_idx
      do ip_rank = 0, ndomain_sph-1
        call const_sph_rtp_grids(ip_rank, ndomain_sph, comm_rtm)
      end do
      call deallocate_rtp_1d_local_idx
!
      end subroutine gen_sph_rtp_grids
!
! ----------------------------------------------------------------------
!
      subroutine gen_fem_mesh_for_sph(ndomain_sph)
!
      use m_gauss_points
      use m_group_data_sph_specr
      use m_sph_mesh_1d_connect
      use const_1d_ele_connect_4_sph
      use gen_sph_grids_modes
      use set_local_index_table_sph
      use set_sph_groups
!
      integer(kind = kint), intent(in) :: ndomain_sph
      integer(kind = kint) :: ip_rank
!
!
      if(iflag_excluding_FEM_mesh .gt. 0) return
!
      call allocate_gauss_points(sph_rtp1%nidx_global_rtp(2))
      call allocate_gauss_colatitude
      call construct_gauss_coefs
      call set_gauss_colatitude
!
      call s_const_1d_ele_connect_4_sph                                 &
     &   (iflag_shell_mode, m_folding, sph_rtp1)
      call set_rj_radial_grp
!
      do ip_rank = 0, ndomain_sph-1
        call const_fem_mesh_for_sph(ip_rank)
      end do
!
      call deallocate_grp_type(radial_rj_grp1)
      call deallocate_gauss_points
      call deallocate_gauss_colatitude
!
      end subroutine gen_fem_mesh_for_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_all_comm_stacks_rlm(ndomain_sph, comm_rlm)
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(inout) :: comm_rlm(ndomain_sph)
      integer(kind = kint) :: ip
!
!
      do ip = 1, ndomain_sph
        call dealloc_type_sph_comm_stack(comm_rlm(ip))
          comm_rlm(ip)%nneib_domain = 0
      end do
!
      end subroutine dealloc_all_comm_stacks_rlm
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_all_comm_stacks_rtm(ndomain_sph, comm_rtm)
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(inout) :: comm_rtm(ndomain_sph)
      integer(kind = kint) :: ip
!
!
      do ip = 1, ndomain_sph
        call dealloc_type_sph_comm_stack(comm_rtm(ip))
        comm_rtm(ip)%nneib_domain = 0
      end do
!
      end subroutine dealloc_all_comm_stacks_rtm
!
! ----------------------------------------------------------------------
!
      end module single_gen_sph_grids_modes
