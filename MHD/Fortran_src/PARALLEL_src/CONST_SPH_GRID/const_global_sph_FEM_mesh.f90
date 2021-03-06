!>@file   const_global_sph_FEM_mesh.f90
!!@brief  module const_global_sph_FEM_mesh
!!
!!@author H.Matsui
!!@date     Programmed by H.Matsui in March, 2015
!
!>@brief  Construct FEM mesh from spherical harmonics transform data
!!
!!@verbatim
!!      subroutine const_global_sph_FEM(sph_rtp, sph_rj, gen_sph)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(construct_spherical_grid), intent(inout) :: gen_sph
!!@endverbatim
!
      module const_global_sph_FEM_mesh
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use calypso_mpi
      use t_file_IO_parameter
      use t_spheric_parameter
      use t_mesh_data
      use t_group_data
      use t_gauss_points
      use t_const_spherical_grid
      use t_sph_local_parameter
      use t_sph_mesh_1d_connect
!
      implicit none
!
      type(sph_local_1d_param), save, private :: sph_lc1_SF
      type(sph_local_default_BC), save, private :: sph_dbc_SF
!
      private :: const_global_rtp_mesh
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      subroutine const_global_sph_FEM(sph_rtp, sph_rj, gen_sph)
!
      use set_sph_1d_domain_id
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
!
      type(construct_spherical_grid), intent(inout) :: gen_sph
!
!
      if(iflag_debug .gt. 0) write(*,*) 'const_global_rtp_mesh'
      call const_global_rtp_mesh(sph_rtp, gen_sph%radial_rtp_grp_lc,    &
     &    gen_sph%s3d_ranks, sph_dbc_SF, gen_sph%sph_lcp,               &
     &    gen_sph%stk_lc1d, gen_sph%sph_gl1d)
!
      call alloc_sph_1d_domain_id(sph_rtp, sph_rj, gen_sph%s3d_ranks)
!
      if(iflag_debug .gt. 0) write(*,*) 'set_sph_1d_domain_id_rtp'
      call set_sph_1d_domain_id_rtp                                     &
     &   (gen_sph%stk_lc1d, gen_sph%sph_gl1d, gen_sph%s3d_ranks)
!
      if(iflag_debug .gt. 0) then
        write(50,*) 'idx_global_rtp_r',                                 &
     &                     gen_sph%sph_gl1d%idx_global_rtp_r
      end if
!
      end subroutine const_global_sph_FEM
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine const_global_rtp_mesh(sph_rtp, radial_rtp_grp,         &
     &          s3d_ranks, sph_dbc, sph_lcp, stk_lc1d, sph_gl1d)
!
      use calypso_mpi_int
      use const_global_sph_grids_modes
      use set_global_spherical_param
      use transfer_to_long_integers
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(group_data), intent(in) :: radial_rtp_grp
!
      type(spheric_global_rank), intent(inout) :: s3d_ranks
      type(sph_local_default_BC), intent(inout) :: sph_dbc
      type(sph_local_parameters), intent(inout) :: sph_lcp
      type(sph_1d_index_stack), intent(inout) :: stk_lc1d
      type(sph_1d_global_index), intent(inout) :: sph_gl1d
!
      integer(kind = kint) :: ist, ip, inc_r, inc_t
      integer(kind = kint) :: igrp, inum, inod
      integer :: ip_rank
!
!
      call calypso_mpi_allreduce_int                                    &
     &   (sph_rtp%irank_sph_rtp, s3d_ranks%ndomain_rtp,                 &
     &    cast_long(3), MPI_MAX)
      s3d_ranks%ndomain_rtp(1:3) = s3d_ranks%ndomain_rtp(1:3) + 1
!
      s3d_ranks%ndomain_sph = nprocs
      call alloc_sph_ranks(s3d_ranks)
      call alloc_sph_gl_parameter(s3d_ranks, sph_lcp)
!
      s3d_ranks%iglobal_rank_rtp(1:3,my_rank)                           &
     &           = sph_rtp%irank_sph_rtp(1:3)
      do ip_rank = 0, nprocs-1
        call calypso_mpi_bcast_int                                      &
     &     (s3d_ranks%iglobal_rank_rtp(1,ip_rank),                      &
     &      cast_long(3), ip_rank)
      end do
      if(s3d_ranks%iglobal_rank_rtp(1,1)                                &
     &       .eq. s3d_ranks%iglobal_rank_rtp(1,0)) then
        inc_r = s3d_ranks%ndomain_rtp(2)
      else
        inc_r = 1
      end if
      if(s3d_ranks%iglobal_rank_rtp(2,1)                                &
     &       .eq. s3d_ranks%iglobal_rank_rtp(2,0)) then
        inc_t = s3d_ranks%ndomain_rtp(1)
      else
        inc_t = 1
      end if
!
      call alloc_nidx_local(s3d_ranks, sph_lc1_SF)
      call alloc_sph_1d_global_stack(s3d_ranks, stk_lc1d)
!
      ip = sph_rtp%irank_sph_rtp(1) + 1
      sph_lc1_SF%nidx_local_rtp_r(ip)= sph_rtp%nidx_rtp(1)
      stk_lc1d%istack_idx_local_rtp_r(ip-1) = sph_rtp%ist_rtp(1) - 1
      stk_lc1d%istack_idx_local_rtp_r(ip) =   sph_rtp%ied_rtp(1)
      do ip = 1, s3d_ranks%ndomain_rtp(1)
        ip_rank = int((ip-1) * inc_r)
        call calypso_mpi_bcast_int(sph_lc1_SF%nidx_local_rtp_r(ip),     &
     &      cast_long(1), ip_rank)
        call calypso_mpi_bcast_int                                      &
     &     (stk_lc1d%istack_idx_local_rtp_r(ip-1),                      &
     &      cast_long(2), ip_rank)
      end do
!
      ip = sph_rtp%irank_sph_rtp(2) + 1
      sph_lc1_SF%nidx_local_rtp_t(ip)= sph_rtp%nidx_rtp(2)
      stk_lc1d%istack_idx_local_rtp_t(ip-1) = sph_rtp%ist_rtp(2) - 1
      stk_lc1d%istack_idx_local_rtp_t(ip) =   sph_rtp%ied_rtp(2)
!
      do ip = 1, s3d_ranks%ndomain_rtp(2)
        ip_rank = int((ip-1) * inc_t)
        call calypso_mpi_bcast_int(sph_lc1_SF%nidx_local_rtp_t(ip),     &
     &      cast_long(1), ip_rank)
        call calypso_mpi_bcast_int                                      &
     &     (stk_lc1d%istack_idx_local_rtp_t(ip-1),                      &
     &      cast_long(2), ip_rank)
      end do
!
      ip = sph_rtp%irank_sph_rtp(3) + 1
      sph_lc1_SF%nidx_local_rtp_p(ip)= sph_rtp%nidx_rtp(3)
      stk_lc1d%istack_idx_local_rtp_p(ip-1) = sph_rtp%ist_rtp(3) - 1
      stk_lc1d%istack_idx_local_rtp_p(ip) =   sph_rtp%ied_rtp(3)
!
!
!
      call alloc_sph_gl_bc_param(s3d_ranks, sph_dbc)
!
      ip = sph_rtp%irank_sph_rtp(1) + 1
      do igrp = 1, radial_rtp_grp%num_grp
        if(radial_rtp_grp%grp_name(igrp) .eq. OC_ele_grp_name) then
          sph_dbc%nidx_local_rtp_OC(ip)                                 &
     &         =  radial_rtp_grp%istack_grp(igrp)                       &
     &           - radial_rtp_grp%istack_grp(igrp-1)
          ist = radial_rtp_grp%istack_grp(igrp-1) + 1
          inum = radial_rtp_grp%item_grp(ist)
          sph_dbc%ist_idx_local_rtp_OC(ip)                              &
     &         = sph_rtp%idx_gl_1d_rtp_r(inum) - 1
          exit
        end if
      end do
!
      do igrp = 1, radial_rtp_grp%num_grp
        if(radial_rtp_grp%grp_name(igrp) .eq. IC_ele_grp_name) then
          sph_dbc%nidx_local_rtp_IC(ip)                                 &
     &         =  radial_rtp_grp%istack_grp(igrp)                       &
     &          - radial_rtp_grp%istack_grp(igrp-1)
          ist = radial_rtp_grp%istack_grp(igrp-1) + 1
          inum = radial_rtp_grp%item_grp(ist)
          sph_dbc%ist_idx_local_rtp_IC(ip)                              &
     &         = sph_rtp%idx_gl_1d_rtp_r(inum) - 1
          exit
        end if
      end do
!
      sph_dbc%nidx_local_rtp_MT(ip) =  sph_rtp%nidx_rtp(1)              &
     &                               - sph_dbc%nidx_local_rtp_OC(ip)    &
     &                               - sph_dbc%nidx_local_rtp_IC(ip)
      if(sph_dbc%nidx_local_rtp_MT(ip) .gt. 0) then
        do igrp = 1, radial_rtp_grp%num_grp
          if(radial_rtp_grp%grp_name(igrp) .eq. OC_ele_grp_name) then
            ist = radial_rtp_grp%istack_grp(igrp)
            inum = radial_rtp_grp%item_grp(ist) + 1
            sph_dbc%ist_idx_local_rtp_MT(ip)                            &
     &            = sph_rtp%idx_gl_1d_rtp_r(inum) - 1
            exit
          end if
        end do
      end if
!
      do ip = 1, s3d_ranks%ndomain_rtp(1)
        ip_rank = int((ip-1) * inc_r)
        call calypso_mpi_bcast_int                                      &
     &     (sph_dbc%nidx_local_rtp_OC(ip), cast_long(1),  ip_rank)
        call calypso_mpi_bcast_int                                      &
     &     (sph_dbc%nidx_local_rtp_IC(ip), cast_long(1),  ip_rank)
        call calypso_mpi_bcast_int                                      &
     &     (sph_dbc%nidx_local_rtp_MT(ip), cast_long(1),  ip_rank)
        call calypso_mpi_bcast_int                                      &
     &     (sph_dbc%ist_idx_local_rtp_OC(ip), cast_long(1), ip_rank)
        call calypso_mpi_bcast_int                                      &
     &     (sph_dbc%ist_idx_local_rtp_IC(ip), cast_long(1), ip_rank)
        call calypso_mpi_bcast_int                                      &
     &     (sph_dbc%ist_idx_local_rtp_MT(ip), cast_long(1), ip_rank)
      end do
!
      call set_gl_nnod_spherical(s3d_ranks%ndomain_sph,                 &
     &    s3d_ranks%ndomain_rtp(1), s3d_ranks%ndomain_rtp(2),           &
     &    s3d_ranks%ndomain_rtp(3), s3d_ranks%iglobal_rank_rtp,         &
     &    sph_lc1_SF%nidx_local_rtp_r, sph_lc1_SF%nidx_local_rtp_t,     &
     &    sph_lc1_SF%nidx_local_rtp_p, sph_lcp%nidx_local_rtp,          &
     &    sph_lcp%nnod_local_rtp)
!
      call alloc_sph_1d_global_idx(s3d_ranks, stk_lc1d, sph_gl1d)
!
      do inum = 1, sph_rtp%nidx_rtp(1)
        inod = sph_rtp%ist_rtp(1) + inum - 1
        sph_gl1d%idx_global_rtp_r(inod) = sph_rtp%idx_gl_1d_rtp_r(inum)
      end do
      do ip = 1, s3d_ranks%ndomain_rtp(1)
        ip_rank = int((ip-1) * inc_r)
        ist = stk_lc1d%istack_idx_local_rtp_r(ip-1) + 1
        call calypso_mpi_bcast_int(sph_gl1d%idx_global_rtp_r(ist),      &
     &      cast_long(sph_lc1_SF%nidx_local_rtp_r(ip)), ip_rank)
      end do
!
      do inum = 1, sph_rtp%nidx_rtp(2)
        inod = sph_rtp%ist_rtp(2) + inum - 1
        sph_gl1d%idx_global_rtp_t(inod) = sph_rtp%idx_gl_1d_rtp_t(inum)
      end do
      do ip = 1, s3d_ranks%ndomain_rtp(2)
        ip_rank = int((ip-1) * inc_t)
        ist = stk_lc1d%istack_idx_local_rtp_t(ip-1) + 1
        call calypso_mpi_bcast_int(sph_gl1d%idx_global_rtp_t(ist),      &
     &      cast_long(sph_lc1_SF%nidx_local_rtp_t(ip)), ip_rank)
      end do
!
      do inod = 1, sph_rtp%nidx_rtp(3)
        sph_gl1d%idx_global_rtp_p(inod,1)                               &
     &        = sph_rtp%idx_gl_1d_rtp_p(inod,1)
        sph_gl1d%idx_global_rtp_p(inod,2)                               &
     &        = sph_rtp%idx_gl_1d_rtp_p(inod,2)
      end do
!
      call dealloc_nidx_local(sph_lc1_SF)
      call dealloc_sph_gl_bc_param(sph_dbc)
!
      end subroutine const_global_rtp_mesh
!
! -----------------------------------------------------------------------
!
      end module const_global_sph_FEM_mesh
