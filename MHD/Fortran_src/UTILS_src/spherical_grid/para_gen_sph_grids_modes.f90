!>@file   para_gen_sph_grids_modes.f90
!!@brief  module para_gen_sph_grids_modes
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2012
!
!>@brief  Set global spherical harmonics indices in local array
!!        (Parallellized version)
!!
!!
!!@verbatim
!!      subroutine para_gen_sph_transfer_grids
!!      subroutine para_gen_sph_modes_grids
!!      subroutine para_gen_fem_mesh_for_sph
!!@endverbatim
!
      module para_gen_sph_grids_modes
!
      use m_precision
!
      use m_machine_parameter
      use m_work_time
      use calypso_mpi
!
      use set_local_sphere_by_global
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine para_gen_sph_transfer_grids
!
      use m_spheric_parameter
      use m_sph_trans_comm_table
      use m_parallel_sph_grids
      use load_data_for_sph_IO
      use gen_sph_grids_modes
!
      integer(kind = kint) :: ip_rank
!
!
      if(iflag_memory_conserve_sph .eq. 0) then 
        call alloc_parallel_sph_grids(ndomain_sph)
      end if
!
      do ip_rank = 0, ndomain_sph-1
!
        if(mod(ip_rank,nprocs) .eq. my_rank                             &
     &    .or. iflag_memory_conserve_sph .eq. 0) then
          if(iflag_debug .gt. 0) write(*,*)                             &
     &               'start table generation for',                      &
     &              ip_rank, 'on ', my_rank, nprocs
          call const_transform_grids_modes(ip_rank)
!
          if(mod(ip_rank,nprocs).eq. my_rank) then
            if(iflag_debug .gt. 0) write(*,*)                           &
     &            'Write spherical transform table for domain ',        &
     &            ip_rank,  ' on ', my_rank
!
            if(iflag_debug .gt. 0) write(*,*)                           &
     &          'output_modes_rlm_sph_trans', ip_rank
            call output_modes_rlm_sph_trans(ip_rank)
            if(iflag_debug .gt. 0) write(*,*)                           &
     &          'output_geom_rtm_sph_trans', ip_rank
            call output_geom_rtm_sph_trans(ip_rank)
          else
            call deallocate_sph_comm_item_rlm
            call deallocate_sph_1d_index_rlm
            call deallocate_spheric_param_rlm
!
            call deallocate_sph_comm_item_rtm
            call deallocate_sph_1d_index_rtm
            call deallocate_spheric_param_rtm
          end if
!
        end if
!
        write(*,'(a,i6,a)') 'Spherical transform table for domain',     &
     &          ip_rank, ' is done.'
      end do
!
      if(iflag_memory_conserve_sph .gt. 0) then
        write(*,*) 'barrier for rlm', my_rank
        call calypso_MPI_barrier
      end if
!
      end subroutine para_gen_sph_transfer_grids
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_modes_grids
!
      use m_spheric_parameter
      use m_parallel_sph_grids
      use set_local_index_table_sph
      use gen_sph_grids_modes
!
      integer(kind = kint) :: ip_rank
!
!
      call allocate_rtp_1d_local_idx
      call allocate_rj_1d_local_idx
!
      do ip_rank = 0, ndomain_sph-1
        if(mod(ip_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct spherical modes and grids for domain ',   &
     &            ip_rank,  ' on ', my_rank
        call const_sph_modes_grids(ip_rank)
      end do
!
      call deallocate_rtp_1d_local_idx
      call deallocate_rj_1d_local_idx
!
      end subroutine para_gen_sph_modes_grids
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_fem_mesh_for_sph
!
      use m_gauss_points
      use m_group_data_sph_specr
      use m_sph_mesh_1d_connect
      use set_local_index_table_sph
      use set_sph_groups
      use gen_sph_grids_modes
!
      integer(kind = kint) :: ip_rank
!
!
      if(iflag_excluding_FEM_mesh .gt. 0) return
!
      n_point = nidx_global_rtp(2)
      call allocate_gauss_points
      call allocate_gauss_colatitude
      call construct_gauss_coefs
      call set_gauss_colatitude
!
      call set_rj_radial_grp
!
      do ip_rank = 0, ndomain_sph-1
        if(mod(ip_rank,nprocs) .ne. my_rank) cycle
!
        if(iflag_debug .gt. 0) write(*,*)                               &
     &             'Construct FEM mesh for domain ', ip_rank,           &
     &             ' on ', my_rank
!
        call const_fem_mesh_for_sph(ip_rank)
      end do
!
      call deallocate_rj_r_grp_item
      call deallocate_gauss_points
      call deallocate_gauss_colatitude
!
      end subroutine para_gen_fem_mesh_for_sph
!
! ----------------------------------------------------------------------
!
      end module para_gen_sph_grids_modes
