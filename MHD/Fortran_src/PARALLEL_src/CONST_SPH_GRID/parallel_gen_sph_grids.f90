!>@file   parallel_gen_sph_grids.f90
!!@brief  module parallel_gen_sph_grids
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2010
!
!>@brief  Main loop to generate spherical harmonics indices
!!
!!@verbatim
!!      subroutine para_gen_sph_grids(sph)
!!      subroutine deallocate_gen_mesh_params
!!        type(sph_grids), intent(inout) :: sph
!!@endverbatim
!
      module parallel_gen_sph_grids
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use calypso_mpi
!
      use m_work_time
!
      use t_spheric_parameter
      use t_sph_trans_comm_tbl
!
      implicit none
!
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rlm_mul(:)
!>      Structure for parallel spherical mesh table
      type(sph_comm_tbl), allocatable :: comm_rtm_mul(:)
!
      integer(kind = kint), allocatable :: nneib_rtm_lc(:)
      integer(kind = kint), allocatable :: nneib_rtm_gl(:)
!
      private :: comm_rlm_mul, comm_rtm_mul
      private :: nneib_rtm_lc, nneib_rtm_gl
!
      private :: allocate_nneib_sph_rtm_tmp
      private :: deallocate_nneib_sph_rtm_tmp
      private :: bcast_comm_stacks_sph
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine para_gen_sph_grids(sph)
!
      use m_spheric_global_ranks
      use set_global_spherical_param
      use para_gen_sph_grids_modes
      use mpi_gen_sph_grids_modes
      use set_comm_table_rtp_rj
      use const_global_sph_grids_modes
      use const_sph_radial_grid
!
      type(sph_grids), intent(inout) :: sph
!
!
!  =========  Set global resolutions ===================================
!
      call set_global_sph_resolution                                    &
     &   (sph%sph_params%l_truncation, sph%sph_params%m_folding,        &
     &    sph%sph_rtp, sph%sph_rtm, sph%sph_rlm, sph%sph_rj)
!
      if(my_rank .eq. 0) then
        call check_global_spheric_parameter                             &
     &     (sph%sph_params, sph%sph_rtp)
        call output_set_radial_grid                                     &
     &     (sph%sph_params, sph%sph_rtp)
      end if
!
!  ========= Generate spherical harmonics table ========================
!
      call s_const_global_sph_grids_modes                               &
     &   (sph%sph_params, sph%sph_rtp, sph%sph_rtm, sph%sph_rj)
!
      call start_eleps_time(2)
      allocate(comm_rlm_mul(ndomain_sph))
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rlm_grids'
        call mpi_gen_sph_rlm_grids                                      &
     &     (sph%sph_params, sph%sph_rlm, comm_rlm_mul)
      if(ndomain_sph .eq. nprocs) then
      else
        call para_gen_sph_rlm_grids                                     &
     &     (ndomain_sph, sph%sph_params, sph%sph_rlm, comm_rlm_mul)
      end if
      call bcast_comm_stacks_sph(ndomain_sph, comm_rlm_mul)
      call end_eleps_time(2)
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rj_modes'
      call start_eleps_time(3)
      if(ndomain_sph .eq. nprocs) then
        call mpi_gen_sph_rj_modes                                       &
     &     (comm_rlm_mul, sph%sph_params, sph%sph_rlm, sph%sph_rj)
      else
        call para_gen_sph_rj_modes(ndomain_sph, comm_rlm_mul,           &
     &      sph%sph_params, sph%sph_rlm, sph%sph_rj)
      end if
      call dealloc_comm_stacks_sph(ndomain_sph, comm_rlm_mul)
      deallocate(comm_rlm_mul)
      call end_eleps_time(3)
!
      call start_eleps_time(2)
      allocate(comm_rtm_mul(ndomain_sph))
!
      if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rtm_grids'
      if(ndomain_sph .eq. nprocs) then
        call mpi_gen_sph_rtm_grids                                      &
     &     (sph%sph_params, sph%sph_rtm, comm_rtm_mul)
      else
        call para_gen_sph_rtm_grids                                     &
     &     (ndomain_sph, sph%sph_params, sph%sph_rtm, comm_rtm_mul)
      end if
      call bcast_comm_stacks_sph(ndomain_sph, comm_rtm_mul)
      call end_eleps_time(2)
!
      call start_eleps_time(3)
      if(ndomain_sph .eq. nprocs) then
        call mpi_gen_sph_rtp_grids                                      &
     &     (comm_rtm_mul, sph%sph_params, sph%sph_rtp, sph%sph_rtm)
      else
        if(iflag_debug .gt. 0) write(*,*) 'para_gen_sph_rtp_grids'
        call para_gen_sph_rtp_grids(ndomain_sph, comm_rtm_mul,          &
     &      sph%sph_params, sph%sph_rtp, sph%sph_rtm)
      end if
      call dealloc_comm_stacks_sph(ndomain_sph, comm_rtm_mul)
!
      deallocate(comm_rtm_mul)
      call calypso_MPI_barrier
      call end_eleps_time(3)
!
      end subroutine para_gen_sph_grids
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_gen_mesh_params
!
      use m_spheric_global_ranks
      use m_sph_global_parameter
      use m_sph_1d_global_index
!
!
      call deallocate_sph_ranks
      call deallocate_sph_1d_domain_id
!
      call deallocate_sph_gl_bc_param
      call deallocate_sph_gl_parameter
!
      call deallocate_sph_1d_global_idx
      call deallocate_sph_1d_global_stack
!
      end subroutine deallocate_gen_mesh_params
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bcast_comm_stacks_sph(ndomain_sph, comm_sph)
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(inout) :: comm_sph(ndomain_sph)
!
      integer(kind = kint) :: ip, iroot
      integer(kind = kint) :: iflag, i
      type(sph_comm_tbl) :: comm_tmp
!
!
!      if(i_debug .gt. 0) write(*,*) 'barrier', my_rank
      call calypso_MPI_barrier
      if(my_rank .eq. 0) write(*,*) 'barrier finished'
!
      call allocate_nneib_sph_rtm_tmp(ndomain_sph)
      do ip = 1, ndomain_sph
        if(mod(ip-1,nprocs) .eq. my_rank) then
          nneib_rtm_lc(ip) = comm_sph(ip)%nneib_domain
        end if
      end do
!
      call MPI_allREDUCE(nneib_rtm_lc(1), nneib_rtm_gl(1),              &
     &      ndomain_sph, CALYPSO_INTEGER, MPI_SUM,                      &
     &      CALYPSO_COMM, ierr_MPI)
!
      do ip = 1, ndomain_sph
        iroot = mod(ip-1,nprocs)
        comm_tmp%nneib_domain = nneib_rtm_gl(ip)
        call alloc_type_sph_comm_stack(comm_tmp)
!
        if(iroot .eq. my_rank) then
          comm_tmp%id_domain(1:comm_sph(ip)%nneib_domain)               &
     &       = comm_sph(ip)%id_domain(1:comm_sph(ip)%nneib_domain)
          comm_tmp%istack_sr(0:comm_sph(ip)%nneib_domain)               &
     &       = comm_sph(ip)%istack_sr(0:comm_sph(ip)%nneib_domain)
        end if
!
        call MPI_Bcast(comm_tmp%id_domain(1), comm_tmp%nneib_domain,    &
     &      CALYPSO_INTEGER, iroot, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(comm_tmp%istack_sr(1), comm_tmp%nneib_domain,    &
     &      CALYPSO_INTEGER, iroot, CALYPSO_COMM, ierr_MPI)
!
        iflag = 0
        do i = 1, comm_tmp%nneib_domain
          if(mod(comm_tmp%id_domain(i),nprocs) .eq. my_rank) then
            iflag = 1
            exit
          end if
        end do
!
        if(iflag .eq. 0) then
          comm_sph(ip)%nneib_domain = 0
        else if(iroot .ne. my_rank) then
!          write(*,*) 'allocate rtm:', my_rank, ip
          comm_sph(ip)%nneib_domain = comm_tmp%nneib_domain
          call alloc_type_sph_comm_stack(comm_sph(ip))
          comm_sph(ip)%id_domain(1:comm_sph(ip)%nneib_domain)           &
     &       = comm_tmp%id_domain(1:comm_sph(ip)%nneib_domain)
          comm_sph(ip)%istack_sr(0:comm_sph(ip)%nneib_domain)           &
     &       = comm_tmp%istack_sr(0:comm_sph(ip)%nneib_domain)
        end if
!
        call dealloc_type_sph_comm_stack(comm_tmp)
      end do
      call deallocate_nneib_sph_rtm_tmp
!
!      do ip = 1, ndomain_sph
!        write(50+my_rank,*) 'ip', ip
!        write(50+my_rank,*) 'nneib_domain', comm_sph(ip)%nneib_domain
!        write(50+my_rank,*) 'id_domain', comm_sph(ip)%id_domain
!      end do
!
      end subroutine bcast_comm_stacks_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_comm_stacks_sph(ndomain_sph, comm_rtm)
!
      integer(kind = kint), intent(in) :: ndomain_sph
      type(sph_comm_tbl), intent(inout) :: comm_rtm(ndomain_sph)
      integer(kind = kint) :: ip, iflag, i, irank_tgt
!
!
      do ip = 1, ndomain_sph
        iflag = 0
        do i = 1, comm_rtm(ip)%nneib_domain
          irank_tgt = comm_rtm(ip)%id_domain(i)
          if(mod(irank_tgt,nprocs) .eq. my_rank) then
            iflag = 1
            exit
          end if
        end do
!
        if(iflag .gt. 0) then
!          write(*,*) 'deallocate rtm:', my_rank, ip
          call dealloc_type_sph_comm_stack(comm_rtm(ip))
          comm_rtm(ip)%nneib_domain = 0
        end if
      end do
!
      end subroutine dealloc_comm_stacks_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_nneib_sph_rtm_tmp(ndomain_sph)
!
      integer(kind = kint), intent(in) :: ndomain_sph
!
      allocate(nneib_rtm_lc(ndomain_sph))
      allocate(nneib_rtm_gl(ndomain_sph))
      nneib_rtm_lc = 0
      nneib_rtm_gl = 0
!
      end subroutine allocate_nneib_sph_rtm_tmp
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_nneib_sph_rtm_tmp
!
!
      deallocate(nneib_rtm_lc, nneib_rtm_gl)
!
      end subroutine deallocate_nneib_sph_rtm_tmp
!
! -----------------------------------------------------------------------
!
      end module parallel_gen_sph_grids
