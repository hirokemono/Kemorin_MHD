!>@file   const_FEM_mesh_sph_mhd.f90
!!@brief  module const_FEM_mesh_sph_mhd
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Construct FEM mesh from spherical harmonics transform data
!!
!!@verbatim
!!      subroutine const_FEM_mesh_4_sph_mhd                             &
!!     &         (sph_params, sph_rtp, sph_rj, radial_rtp_grp,          &
!!     &          radial_rj_grp, mesh, group, mesh_file,                &
!!     &          stbl, stk_lc1d, sph_gl1d)
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(group_data), intent(in) :: radial_rtp_grp, radial_rj_grp
!!        type(mesh_geometry), intent(inout) :: mesh
!!        type(mesh_groups), intent(inout) ::  group
!!        type(field_IO_params), intent(inout) ::  mesh_file
!!        type(comm_table_make_sph), intent(inout) :: stbl
!!        type(sph_1d_index_stack), intent(inout) :: stk_lc1d
!!        type(sph_1d_global_index), intent(inout) :: sph_gl1d
!!@endverbatim
!
      module const_FEM_mesh_sph_mhd
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_file_IO_parameter
      use t_spheric_parameter
      use t_mesh_data
      use t_group_data
      use t_gauss_points
      use t_sph_mesh_1d_connect
      use t_sph_1d_global_index
!
      implicit none
!
      type(gauss_points), private :: gauss_s
!
      private :: const_global_sph_FEM, const_global_rtp_mesh
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_FEM_mesh_4_sph_mhd                               &
     &         (sph_params, sph_rtp, sph_rj, radial_rtp_grp,            &
     &          radial_rj_grp, mesh, group, mesh_file,                  &
     &          stbl, stk_lc1d, sph_gl1d)
!
      use calypso_mpi
      use m_spheric_global_ranks
      use set_FEM_mesh_4_sph
      use const_1d_ele_connect_4_sph
      use set_sph_groups
      use gen_sph_grids_modes
      use set_FEM_mesh_4_sph
      use mpi_load_mesh_data
      use sph_file_IO_select
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(in) :: radial_rtp_grp, radial_rj_grp
!
      type(mesh_geometry), intent(inout) :: mesh
      type(mesh_groups), intent(inout) ::  group
      type(field_IO_params), intent(inout) ::  mesh_file
!
      type(comm_table_make_sph), intent(inout) :: stbl
      type(sph_1d_index_stack), intent(inout) :: stk_lc1d
      type(sph_1d_global_index), intent(inout) :: sph_gl1d
!
!
      call const_gauss_colatitude(sph_rtp%nidx_global_rtp(2), gauss_s)
!
!
      call const_global_sph_FEM                                         &
     &   (sph_rtp, sph_rj, radial_rtp_grp, stk_lc1d, sph_gl1d)
      call s_const_1d_ele_connect_4_sph                                 &
     &   (sph_params%iflag_shell_mode, sph_params%m_folding, sph_rtp,   &
     &    stk_lc1d, sph_gl1d, stbl)
!
!      write(*,*) 's_const_FEM_mesh_for_sph',                           &
!     &          sph_params%iflag_shell_mode, iflag_MESH_w_center
      call s_const_FEM_mesh_for_sph                                     &
     &   (my_rank, sph_rtp%nidx_rtp, sph_rj%radius_1d_rj_r, gauss_s,    &
     &    stk_lc1d, sph_gl1d, sph_params, sph_rtp, radial_rj_grp,       &
     &    mesh, group, stbl)
!
! Output mesh data
      if(iflag_output_mesh .gt. 0) then
        mesh_file%file_prefix = sph_file_head
        call mpi_output_mesh(mesh_file, mesh, group)
        write(*,'(a,i6,a)')                                             &
     &          'FEM mesh for domain', my_rank, ' is done.'
      end if
!
      call dealloc_nnod_nele_sph_mesh(stbl)
      call dealloc_gauss_colatitude(gauss_s)
!
      end subroutine const_FEM_mesh_4_sph_mhd
!
!-----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_global_sph_FEM                                   &
     &         (sph_rtp, sph_rj, radial_rtp_grp, stk_lc1d, sph_gl1d)
!
      use m_spheric_global_ranks
      use m_sph_global_parameter
      use set_sph_1d_domain_id
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(in) :: radial_rtp_grp
!
      type(sph_1d_index_stack), intent(inout) :: stk_lc1d
      type(sph_1d_global_index), intent(inout) :: sph_gl1d
!
!
      if(iflag_debug .gt. 0) write(*,*) 'const_global_rtp_mesh'
      call const_global_rtp_mesh                                        &
     &   (sph_rtp, radial_rtp_grp, stk_lc1d, sph_gl1d)
!
      call alloc_sph_1d_domain_id(sph_rtp, sph_rj, s3d_ranks)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_1d_domain_id_rtp'
      call set_sph_1d_domain_id_rtp(stk_lc1d, sph_gl1d)
!
      if(iflag_debug .gt. 0) then
        write(50,*) 'idx_global_rtp_r', sph_gl1d%idx_global_rtp_r
      end if
!
      end subroutine const_global_sph_FEM
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_global_rtp_mesh                                  &
     &         (sph_rtp, radial_rtp_grp, stk_lc1d, sph_gl1d)
!
      use calypso_mpi
      use m_spheric_global_ranks
      use m_sph_global_parameter
      use const_global_sph_grids_modes
      use set_global_spherical_param
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(group_data), intent(in) :: radial_rtp_grp
!
      type(sph_1d_index_stack), intent(inout) :: stk_lc1d
      type(sph_1d_global_index), intent(inout) :: sph_gl1d
!
      integer(kind = kint) :: ist, ip, inc_r, inc_t, ip_rank
      integer(kind = kint) :: igrp, inum, inod
!
!
      call MPI_allREDUCE(sph_rtp%irank_sph_rtp, ndomain_rtp, ithree,    &
     &    CALYPSO_INTEGER, MPI_MAX, CALYPSO_COMM, ierr_MPI)
      ndomain_rtp(1:3) = ndomain_rtp(1:3) + 1
!
      ndomain_sph = nprocs
      call allocate_sph_ranks
      call allocate_sph_gl_parameter
!
      iglobal_rank_rtp(1:3,my_rank) = sph_rtp%irank_sph_rtp(1:3)
      do ip = 0, nprocs-1
        call MPI_Bcast(iglobal_rank_rtp(1,ip), ithree, CALYPSO_INTEGER, &
     &      ip, CALYPSO_COMM, ierr_MPI)
      end do
      if(iglobal_rank_rtp(1,1) .eq. iglobal_rank_rtp(1,0)) then
        inc_r = ndomain_rtp(2)
      else
        inc_r = 1
      end if
      if(iglobal_rank_rtp(2,1) .eq. iglobal_rank_rtp(2,0)) then
        inc_t = ndomain_rtp(1)
      else
        inc_t = 1
      end if
!
      call allocate_nidx_local
      call alloc_sph_1d_global_stack(stk_lc1d)
!
      ip = sph_rtp%irank_sph_rtp(1) + 1
      nidx_local_rtp_r(ip)= sph_rtp%nidx_rtp(1)
      stk_lc1d%istack_idx_local_rtp_r(ip-1) = sph_rtp%ist_rtp(1) - 1
      stk_lc1d%istack_idx_local_rtp_r(ip) =   sph_rtp%ied_rtp(1)
      do ip = 1, ndomain_rtp(1)
        ip_rank = (ip-1) * inc_r
        call MPI_Bcast(nidx_local_rtp_r(ip), ione,                      &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(stk_lc1d%istack_idx_local_rtp_r(ip-1), itwo,     &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
      end do
!
      ip = sph_rtp%irank_sph_rtp(2) + 1
      nidx_local_rtp_t(ip)= sph_rtp%nidx_rtp(2)
      stk_lc1d%istack_idx_local_rtp_t(ip-1) = sph_rtp%ist_rtp(2) - 1
      stk_lc1d%istack_idx_local_rtp_t(ip) =   sph_rtp%ied_rtp(2)
      do ip = 1, ndomain_rtp(2)
        ip_rank = (ip-1) * inc_t
        call MPI_Bcast(nidx_local_rtp_t(ip), ione,                      &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(stk_lc1d%istack_idx_local_rtp_t(ip-1), itwo,     &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
      end do
!
      ip = sph_rtp%irank_sph_rtp(3) + 1
      nidx_local_rtp_p(ip)= sph_rtp%nidx_rtp(3)
      stk_lc1d%istack_idx_local_rtp_p(ip-1) = sph_rtp%ist_rtp(3) - 1
      stk_lc1d%istack_idx_local_rtp_p(ip) =   sph_rtp%ied_rtp(3)
!
!
!
      call allocate_sph_gl_bc_param
!
      ip = sph_rtp%irank_sph_rtp(1) + 1
      do igrp = 1, radial_rtp_grp%num_grp
        if(radial_rtp_grp%grp_name(igrp) .eq. OC_ele_grp_name) then
          nidx_local_rtp_OC(ip) =  radial_rtp_grp%istack_grp(igrp)      &
     &                           - radial_rtp_grp%istack_grp(igrp-1)
          ist = radial_rtp_grp%istack_grp(igrp-1) + 1
          inum = radial_rtp_grp%item_grp(ist)
          ist_idx_local_rtp_OC(ip) = sph_rtp%idx_gl_1d_rtp_r(inum) - 1
          exit
        end if
      end do
!
      do igrp = 1, radial_rtp_grp%num_grp
        if(radial_rtp_grp%grp_name(igrp) .eq. IC_ele_grp_name) then
          nidx_local_rtp_IC(ip) =  radial_rtp_grp%istack_grp(igrp)      &
     &                           - radial_rtp_grp%istack_grp(igrp-1)
          ist = radial_rtp_grp%istack_grp(igrp-1) + 1
          inum = radial_rtp_grp%item_grp(ist)
          ist_idx_local_rtp_IC(ip) = sph_rtp%idx_gl_1d_rtp_r(inum) - 1
          exit
        end if
      end do
!
      nidx_local_rtp_MT(ip) =  sph_rtp%nidx_rtp(1)                      &
     &                        - nidx_local_rtp_OC(ip)                   &
     &                        - nidx_local_rtp_IC(ip)
      if(nidx_local_rtp_MT(ip) .gt. 0) then
        do igrp = 1, radial_rtp_grp%num_grp
          if(radial_rtp_grp%grp_name(igrp) .eq. OC_ele_grp_name) then
            ist = radial_rtp_grp%istack_grp(igrp)
            inum = radial_rtp_grp%item_grp(ist) + 1
            ist_idx_local_rtp_MT(ip)                                    &
     &            = sph_rtp%idx_gl_1d_rtp_r(inum) - 1
            exit
          end if
        end do
      end if
!
      do ip = 1, ndomain_rtp(1)
        ip_rank = (ip-1) * inc_r
        call MPI_Bcast(nidx_local_rtp_OC(ip), ione,                     &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(nidx_local_rtp_IC(ip), ione,                     &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(nidx_local_rtp_MT(ip), ione,                     &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(ist_idx_local_rtp_OC(ip), ione,                  &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(ist_idx_local_rtp_IC(ip), ione,                  &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(ist_idx_local_rtp_MT(ip), ione,                  &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
      end do
!
      call set_gl_nnod_spherical(ndomain_sph,                           &
     &    ndomain_rtp(1), ndomain_rtp(2), ndomain_rtp(3),               &
     &    iglobal_rank_rtp, nidx_local_rtp_r, nidx_local_rtp_t,         &
     &    nidx_local_rtp_p, nidx_local_rtp, nnod_local_rtp)
!
      call alloc_sph_1d_global_idx(stk_lc1d, sph_gl1d)
!
      do inum = 1, sph_rtp%nidx_rtp(1)
        inod = sph_rtp%ist_rtp(1) + inum - 1
        sph_gl1d%idx_global_rtp_r(inod) = sph_rtp%idx_gl_1d_rtp_r(inum)
      end do
      do ip = 1, ndomain_rtp(1)
        ip_rank = (ip-1) * inc_r
        ist = stk_lc1d%istack_idx_local_rtp_r(ip-1) + 1
        call MPI_Bcast                                                  &
     &     (sph_gl1d%idx_global_rtp_r(ist), nidx_local_rtp_r(ip),       &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
      end do
!
      do inum = 1, sph_rtp%nidx_rtp(2)
        inod = sph_rtp%ist_rtp(2) + inum - 1
        sph_gl1d%idx_global_rtp_t(inod) = sph_rtp%idx_gl_1d_rtp_t(inum)
      end do
      do ip = 1, ndomain_rtp(2)
        ip_rank = (ip-1) * inc_t
        ist = stk_lc1d%istack_idx_local_rtp_t(ip-1) + 1
        call MPI_Bcast                                                  &
     &     (sph_gl1d%idx_global_rtp_t(ist), nidx_local_rtp_t(ip),       &
     &      CALYPSO_INTEGER, ip_rank, CALYPSO_COMM, ierr_MPI)
      end do
!
      do inod = 1, sph_rtp%nidx_rtp(3)
        sph_gl1d%idx_global_rtp_p(inod,1)                               &
     &        = sph_rtp%idx_gl_1d_rtp_p(inod,1)
        sph_gl1d%idx_global_rtp_p(inod,2)                               &
     &        = sph_rtp%idx_gl_1d_rtp_p(inod,2)
      end do
!
      call deallocate_nidx_local
!
      end subroutine const_global_rtp_mesh
!
! -----------------------------------------------------------------------
!
      end module const_FEM_mesh_sph_mhd
