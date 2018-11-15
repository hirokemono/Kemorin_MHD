!>@file   analyzer_rayleigh_convert.f90
!!@brief  module analyzer_rayleigh_convert
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in Apr., 2018
!
!>@brief  Main loop to assemble spectr data
!!
!!@verbatim
!!      subroutine init_cvt_rayleigh
!!      subroutine analyze_cvt_rayleigh
!!@endverbatim
!
      module analyzer_rayleigh_convert
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use m_machine_parameter
      use m_file_format_switch
!
      use r_interpolate_marged_sph
      use t_SPH_mesh_field_data
      use t_time_data
      use t_field_data_IO
      use t_control_data_4_merge
      use t_control_param_assemble
      use t_spectr_data_4_assemble
!
      use new_SPH_restart
      use parallel_assemble_sph
      use copy_rj_phys_data_4_IO
      use assemble_sph_fields
      use set_control_newsph
      use rayleigh_restart_IO
      use field_IO_select
!
      implicit none
!
      type work_fftpack_chebyshev
        integer(kind = kint) :: LENSAV
        real(kind = kreal), allocatable :: WSAVE(:)
        real(kind = kreal), allocatable :: WORK(:)
      end type work_fftpack_chebyshev
!
      type work_rayleigh_checkpoint
        integer(kind = kint) :: nri_tgt
        real(kind = kreal), allocatable :: rayleigh_in(:,:)
        real(kind = kreal), allocatable :: rayleigh_tg(:,:)
        real(kind = kreal), allocatable :: rayleigh_fd(:,:)
      end type work_rayleigh_checkpoint
!
      type(control_data_4_merge), save :: mgd_ctl_s
      type(control_param_assemble), save :: asbl_param_s
      type(spectr_data_4_assemble), save :: sph_asbl_s
      type(time_data), save :: init_t
!
      type(rayleigh_restart), save :: ra_rst_s
      type(field_IO), save :: fld_IO_r
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine init_cvt_rayleigh
!
      use m_error_IDs
!
      use bcast_4_assemble_sph_ctl
      use sph_file_MPI_IO_select
      use sph_file_IO_select
      use field_IO_select
      use share_spectr_index_data
      use count_nnod_4_asseble_sph
!
      use share_field_data
!
!
      write(*,*) 'Simulation start: PE. ', my_rank
!
      if(my_rank .eq. 0) call read_control_assemble_sph(mgd_ctl_s)
      call bcast_merge_control_data(mgd_ctl_s)
      call set_control_4_newsph(mgd_ctl_s, asbl_param_s, sph_asbl_s)
!
      sph_asbl_s%np_sph_org = 1
      call alloc_spectr_data_4_assemble(sph_asbl_s)
!
!  set original spectr data
!
      if(asbl_param_s%org_fld_file%iflag_format .eq. id_rayleigh) then
        call init_rayleigh_restart_params                               &
     &     (ra_rst_s, sph_asbl_s%org_sph_mesh(1))
      end if
!
!  set new spectr data
!
      call set_local_rj_mesh_4_merge(asbl_param_s%new_mesh_file,        &
     &    sph_asbl_s%np_sph_new, sph_asbl_s%new_sph_mesh)
!
!     Share number of nodes for new mesh
!
      call s_count_nnod_4_asseble_sph(sph_asbl_s%np_sph_new,            &
     &   sph_asbl_s%new_sph_mesh, sph_asbl_s%new_fst_IO)
!
!     construct radial interpolation table
!
      call const_r_interpolate_table                                    &
     &   (sph_asbl_s%org_sph_mesh(1), sph_asbl_s%new_sph_mesh(1),       &
     &    sph_asbl_s%r_itp)
!
!      call chebyshev_fwd_mat_4_rayleigh                                &
!     &   (sph_asbl_s%new_sph_mesh(1), sph_asbl_s%r_itp, ra_rst_s)
!
      call dealloc_rayleigh_radial_grid(ra_rst_s)
!
!      Construct field list from spectr file
!
      call init_rayleigh_restart_input                                  &
     &     (asbl_param_s%org_fld_file%file_prefix,                      &
     &      asbl_param_s%istep_start, fld_IO_r)
!
      call chech_field_name_4_IO(50+my_rank, fld_IO_r)
!
      if(my_rank .eq. 0) then
        call copy_rj_phys_name_from_IO                                  &
     &     (fld_IO_r, sph_asbl_s%new_sph_phys(1))
      end if
      call share_new_spectr_field_names(sph_asbl_s%np_sph_new,          &
     &    sph_asbl_s%new_sph_mesh, sph_asbl_s%new_sph_phys(1))
!
      call check_rayleigh_field_param                                   &
     &   (50+my_rank, sph_asbl_s%new_sph_phys(1))
!
      end subroutine init_cvt_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine analyze_cvt_rayleigh
!
      use r_interpolate_marged_sph
      use set_field_file_names
      use share_field_data
      use matmul_for_legendre_trans
!
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
!
      integer(kind = kint) :: istep
      integer(kind = kint) :: istep_out
!
!     ---------------------
!
      do istep = asbl_param_s%istep_start, asbl_param_s%istep_end,      &
     &          asbl_param_s%increment_step
!
        if(my_rank .eq. 0) then
          call read_rayleigh_restart_params                             &
     &       (asbl_param_s%org_fld_file%file_prefix,                    &
     &        istep, ra_rst_s)
        end if
        call bcast_rayleigh_rst_params(ra_rst_s)
        call dealloc_rayleigh_radial_grid(ra_rst_s)
!
        istep_out =          istep/ asbl_param_s%increment_step
        init_t%i_time_step = istep
        init_t%time =        ra_rst_s%time_org
        call share_time_step_data(init_t)
!
        call convert_fields_from_rayleigh(istep,                        &
     &      sph_asbl_s%new_sph_mesh(my_rank+1), sph_asbl_s%r_itp,       &
     &      ra_rst_s, sph_asbl_s%new_sph_phys(my_rank+1))
!
        call const_assembled_sph_data(asbl_param_s%b_ratio, init_t,     &
     &      sph_asbl_s%new_sph_mesh(my_rank+1)%sph, sph_asbl_s%r_itp,   &
     &      sph_asbl_s%new_sph_phys(my_rank+1),                         &
     &      sph_asbl_s%new_fst_IO, sph_asbl_s%fst_time_IO)
!
!        write(*,*) 'sel_write_step_SPH_field_file'
        call sel_write_step_SPH_field_file                              &
     &     (nprocs, my_rank, istep_out, asbl_param_s%new_fld_file,      &
     &      sph_asbl_s%fst_time_IO, sph_asbl_s%new_fst_IO)
!
        call dealloc_phys_data_IO(sph_asbl_s%new_fst_IO)
        call dealloc_phys_name_IO(sph_asbl_s%new_fst_IO)
      end do
        close(50+my_rank)
      call calypso_MPI_barrier
!
      if (iflag_debug.eq.1) write(*,*) 'exit evolution'
!
      end subroutine analyze_cvt_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine alloc_work_rayleigh_restart                            &
     &         (nri_org, r_itp, rayleigh_WK)
!
      integer(kind = kint), intent(in) :: nri_org
      type(sph_radial_itp_data), intent(in) :: r_itp
      type(work_rayleigh_checkpoint), intent(inout) :: rayleigh_WK
!
!
      rayleigh_WK%nri_tgt = r_itp%kr_outer_domain                       &
     &                     - r_itp%kr_inner_domain + 1
      allocate(rayleigh_WK%rayleigh_in(nri_org,2))
      allocate(rayleigh_WK%rayleigh_tg(rayleigh_WK%nri_tgt+1,1))
      if(nri_org .gt. 0) rayleigh_WK%rayleigh_in = 0.0d0
      rayleigh_WK%rayleigh_tg = 0.0d0
!
      allocate(rayleigh_WK%rayleigh_fd(nri_org,1))
      if(nri_org .gt. 0) rayleigh_WK%rayleigh_fd = 0.0d0
!
      end subroutine alloc_work_rayleigh_restart
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_work_rayleigh_restart(rayleigh_WK)
!
      type(work_rayleigh_checkpoint), intent(inout) :: rayleigh_WK
!
!
        deallocate(rayleigh_WK%rayleigh_in, rayleigh_WK%rayleigh_tg)
        deallocate(rayleigh_WK%rayleigh_fd)
!
      end subroutine dealloc_work_rayleigh_restart
!
! ----------------------------------------------------------------------
!
      subroutine init_fftpack_4_cheby(nri_tgt, fcheby_WK, ierr)
!
      integer(kind = kint), intent(in) :: nri_tgt
      type(work_fftpack_chebyshev), intent(inout) :: fcheby_WK
      integer(kind = kint), intent(inout) :: ierr
!
      fcheby_WK%LENSAV = 2*(nri_tgt+1) + int(log(dble(nri_tgt+1)))+4
      allocate(fcheby_WK%WSAVE(fcheby_WK%LENSAV))
      allocate(fcheby_WK%WORK(nri_tgt+1))
!
      call COST1I(nri_tgt, fcheby_WK%WSAVE, fcheby_WK%LENSAV, ierr)
!
      end subroutine init_fftpack_4_cheby
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_fftpack_4_cheby(fcheby_WK)
!
      type(work_fftpack_chebyshev), intent(inout) :: fcheby_WK
!
!
      deallocate(fcheby_WK%WSAVE, fcheby_WK%WORK)
!
      end subroutine dealloc_fftpack_4_cheby
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine convert_fields_from_rayleigh                           &
     &         (istep, new_sph_mesh, r_itp, ra_rst, new_sph_phys)
!
      integer(kind = kint), intent(in) :: istep
      type(sph_mesh_data), intent(in) :: new_sph_mesh
      type(rayleigh_restart), intent(in) :: ra_rst
      type(sph_radial_itp_data), intent(in) :: r_itp
!
      type(phys_data), intent(inout) :: new_sph_phys
!
      character(len = kchara) :: file_name(2)
      type(work_fftpack_chebyshev) :: fcheby_WK
      type(work_rayleigh_checkpoint) :: rayleigh_WK
!
      integer(kind = kint) :: i_fld, i_comp, nd, iflag_ncomp
      integer(kind = kint) :: ierr
!
!
      call alloc_work_rayleigh_restart                                  &
     &   (ra_rst%nri_org, r_itp, rayleigh_WK)
      call init_fftpack_4_cheby(rayleigh_WK%nri_tgt, fcheby_WK, ierr)
!
      do i_fld = 1, new_sph_phys%num_phys
        call set_rayleigh_rst_file_name                               &
     &     (asbl_param_s%org_fld_file%file_prefix, istep,             &
     &      new_sph_phys%phys_name(i_fld), iflag_ncomp, file_name(1))
!
        do nd = 1, iflag_ncomp
          i_comp = 2*nd - 1 + new_sph_phys%istack_component(i_fld-1)
!
!          call check_rayleigh_restart_reading(file_name(nd), i_comp, &
!     &        ra_rst, rayleigh_WK%rayleigh_in)
          call cvt_each_field_from_rayleigh(file_name(nd),            &
     &        i_fld, i_comp, new_sph_mesh,                            &
     &        r_itp, ra_rst, fcheby_WK, rayleigh_WK, new_sph_phys)
        end do
!
      end do
      call dealloc_fftpack_4_cheby(fcheby_WK)
      call dealloc_work_rayleigh_restart(rayleigh_WK)
!
      end subroutine convert_fields_from_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine cvt_each_field_from_rayleigh                           &
     &          (file_name, i_fld, i_comp, new_sph_mesh, r_itp, ra_rst, &
     &           fcheby_WK, rayleigh_WK, new_sph_phys)
!
      use m_phys_labels
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
!
      integer(kind = kint), intent(in) :: i_fld, i_comp
      character(len = kchara), intent(in) :: file_name
      type(sph_mesh_data), intent(in) :: new_sph_mesh
      type(rayleigh_restart), intent(in) :: ra_rst
      type(sph_radial_itp_data), intent(in) :: r_itp
      type(work_fftpack_chebyshev), intent(in) :: fcheby_WK
!
      type(work_rayleigh_checkpoint), intent(inout) :: rayleigh_WK
      type(phys_data), intent(inout) :: new_sph_phys
!
      type(calypso_MPI_IO_params), save :: IO_param
      integer(kind = kint) :: k, j, l, m, ierr
      integer(kind = MPI_OFFSET_KIND) :: ioffset1, ioffset2
!
!
      call open_read_mpi_file                                           &
     &   (file_name, nprocs, my_rank, IO_param)
!      write(50+my_rank,*) 'k, kr, l, m, ioffset1, ioffset2', &
!       &     new_sph_mesh%sph%sph_rj%nidx_rj(1:2)
      do j = 1, new_sph_mesh%sph%sph_rj%nidx_rj(2)
        l = new_sph_mesh%sph%sph_rj%idx_gl_1d_rj_j(j,2)
        m = new_sph_mesh%sph%sph_rj%idx_gl_1d_rj_j(j,3)
        if(l .gt. ra_rst%ltr_org) cycle
        do k = 1, ra_rst%nri_org
          call find_rayleigh_restart_address                            &
     &       (ra_rst%nri_org, ra_rst%ltr_org,                           &
     &        k, l, abs(m), ioffset1, ioffset2)
!
          call calypso_mpi_seek_read_real                               &
     &       (IO_param%id_file, ra_rst%iflag_swap,                      &
     &        ioffset1, ione, rayleigh_WK%rayleigh_in(k,1))
          call calypso_mpi_seek_read_real                               &
     &       (IO_param%id_file, ra_rst%iflag_swap,                      &
     &        ioffset2, ione, rayleigh_WK%rayleigh_in(k,2))
        end do
!
        call rescaling_from_rayleigh                                    &
     &     (l, m, ra_rst%nri_org, rayleigh_WK%rayleigh_in)
!
!      call matmul_bwd_leg_trans(ra_rst%nri_org, ione, ra_rst%nri_org,  &
!     &    ra_rst%Cheby_fwd(1,1), rayleigh_WK%rayleigh_tg(1,1),         &
!     &    rayleigh_WK%rayleigh_fd(1,1))
!
        if    (new_sph_phys%phys_name(i_fld) .eq. fhd_velo              &
      &   .or. new_sph_phys%phys_name(i_fld) .eq. fhd_press             &
      &   .or. new_sph_phys%phys_name(i_fld) .eq. fhd_temp              &
      &   .or. new_sph_phys%phys_name(i_fld) .eq. fhd_magne) then
          call rescaling_for_chebyshev_FFT                              &
      &      (ra_rst%nri_org, rayleigh_WK%rayleigh_in(1,1),             &
      &       rayleigh_WK%nri_tgt, rayleigh_WK%rayleigh_tg(1,1))
          call COST1B(rayleigh_WK%nri_tgt, ione,                        &
      &       rayleigh_WK%rayleigh_tg(1,1), rayleigh_WK%nri_tgt+1,      &
      &       fcheby_WK%WSAVE, fcheby_WK%LENSAV, fcheby_WK%WORK,        &
      &       rayleigh_WK%nri_tgt+1, ierr)
!
          call copy_from_chebyshev_trans(new_sph_mesh%sph%sph_rj,       &
     &        r_itp, j, i_comp,  rayleigh_WK%nri_tgt,                   &
     &        rayleigh_WK%rayleigh_tg(1,1), new_sph_phys)
       else if(new_sph_phys%phys_name(i_fld) .eq. fhd_pre_mom           &
      &   .or. new_sph_phys%phys_name(i_fld) .eq. fhd_pre_heat          &
      &   .or. new_sph_phys%phys_name(i_fld) .eq. fhd_pre_uxb) then
          call radial_interpolation_rayleigh(r_itp,                     &
     &        ra_rst%nri_org, rayleigh_WK%rayleigh_in(1,1),             &
     &        rayleigh_WK%nri_tgt, rayleigh_WK%rayleigh_tg(1,1))
       end if
!
!          call check_chebyshev_trans                                   &
!     &       (new_sph_mesh%sph%sph_rj, r_itp, file_name,               &
!     &        l, m, j, i_fld, i_comp, ra_rst%nri_org,                  &
!     &        rayleigh_WK%rayleigh_in, rayleigh_WK%nri_tgt,            &
!     &        rayleigh_WK%rayleigh_tg, new_sph_phys)
!        end if
      end do
!
      call close_mpi_file(IO_param)
!
      end subroutine cvt_each_field_from_rayleigh
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
!
      subroutine rescaling_from_rayleigh(l, m, nri_org, rayleigh_in)
!
      use m_precision
      use m_constants
      implicit none
!
      integer(kind = kint), intent(in) :: l, m
      integer(kind = kint), intent(in) :: nri_org
      real(kind = kreal), intent(inout) :: rayleigh_in(nri_org,2)
!
      real(kind= kreal) :: pi
!
!
      pi = four * atan(one)
!
!       Transfer to Schinidt normalization
      if(m .eq. 0) then
        rayleigh_in(1:nri_org,1)                                        &
     &               = rayleigh_in(1:nri_org,1) * sqrt(two)
      else if(m .lt. 0) then
        rayleigh_in(1:nri_org,1) = -rayleigh_in(1:nri_org,2)
!      else if(m .gt. 0) then
!        rayleigh_in(1:nri_org,1) =  rayleigh_in(1:nri_org,1)
      end if
!
      rayleigh_in(1:nri_org,1) = rayleigh_in(1:nri_org,1)               &
     &                            * sqrt(dble(2*l+1) / (two*pi))
!
      end subroutine rescaling_from_rayleigh
!
! -----------------------------------------------------------------------
!
      subroutine rescaling_for_chebyshev_FFT                            &
     &         (nri_org, rayleigh_in, nri_tgt, rayleigh_tg)
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint), intent(in) :: nri_org, nri_tgt
      real(kind = kreal), intent(in) :: rayleigh_in(nri_org)
      real(kind = kreal), intent(inout) :: rayleigh_tg(nri_tgt+1)
!
      integer(kind = kint) :: k, nri_min
!
!
      nri_min = min(nri_org, nri_tgt)
!
!   Normalize for Chebyshev mode 0
      rayleigh_tg(1) = half * half* rayleigh_in(1)
      do k = 2, nri_min
        rayleigh_tg(k) = half * (-one)**(k-1) * rayleigh_in(k)
      end do
!
      end subroutine rescaling_for_chebyshev_FFT
!
! -----------------------------------------------------------------------
!
      subroutine radial_interpolation_rayleigh                          &
     &         (r_itp, nri_org, rayleigh_in, nri_tgt, rayleigh_tg)
!
      type(sph_radial_itp_data), intent(in) :: r_itp
      integer(kind = kint), intent(in) :: nri_org, nri_tgt
      real(kind = kreal), intent(in) :: rayleigh_in(nri_org)
      real(kind = kreal), intent(inout) :: rayleigh_tg(nri_tgt+1)
!
      integer(kind = kint) :: k, kr, kr_in, kr_out
!
!
      do kr = r_itp%kr_inner_domain, r_itp%kr_outer_domain
        k = kr - r_itp%kr_inner_domain + 1
        kr_in =  nri_org - r_itp%k_old2new_in(kr) +  1
        kr_out = nri_org - r_itp%k_old2new_out(kr) + 1
        rayleigh_tg(k) = r_itp%coef_old2new_in(kr) * rayleigh_in(kr_in) &
     &                + (1.0d0 - r_itp%coef_old2new_in(kr))             &
     &                * rayleigh_in(kr_out)
      end do
!
      end subroutine radial_interpolation_rayleigh
!
! -----------------------------------------------------------------------
!
      subroutine copy_from_chebyshev_trans(sph_rj, r_itp, j, i_comp,    &
     &          nri_tgt, rayleigh_tg, new_sph_phys)
!
      use t_spheric_rj_data
      use t_phys_data
      use r_interpolate_marged_sph
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_radial_itp_data), intent(in) :: r_itp
      integer(kind = kint), intent(in) :: i_comp, j
      integer(kind = kint), intent(in) :: nri_tgt
      real(kind = kreal), intent(in) :: rayleigh_tg(nri_tgt+1,1)
!
      type(phys_data), intent(inout) :: new_sph_phys
!
      integer(kind = kint) :: k, kr, inod
!
!
      do k = 1, nri_tgt
        kr = r_itp%kr_inner_domain + k - 1
        inod = j + (kr-1) * sph_rj%nidx_rj(2)
        new_sph_phys%d_fld(inod,i_comp) = rayleigh_tg(k,1)
      end do
!
      end subroutine copy_from_chebyshev_trans
!
! -----------------------------------------------------------------------
!
      subroutine init_rayleigh_restart_params                           &
     &         (ra_rst, org_sph_mesh)
!
      type(rayleigh_restart), intent(inout) :: ra_rst
      type(sph_mesh_data), intent(inout) :: org_sph_mesh
!
      integer(kind = kint) :: k, kr
!
!
      if(my_rank .eq. 0) then
        call read_rayleigh_restart_params                               &
     &       (asbl_param_s%org_fld_file%file_prefix,                    &
     &        asbl_param_s%istep_start, ra_rst)
!
        org_sph_mesh%sph%sph_rj%nidx_rj(1) = ra_rst%nri_org
        org_sph_mesh%sph%sph_rj%nidx_rj(2) = 1
        call alloc_type_sph_1d_index_rj(org_sph_mesh%sph%sph_rj)
        do k = 1, org_sph_mesh%sph%sph_rj%nidx_rj(1)
          kr = ra_rst%nri_org-k+1
          org_sph_mesh%sph%sph_rj%radius_1d_rj_r(k) = ra_rst%r_org(kr)
        end do
!
      end if
      call bcast_rayleigh_rst_params(ra_rst)
!
      end subroutine init_rayleigh_restart_params
!
! -----------------------------------------------------------------------
!
      subroutine chebyshev_fwd_mat_4_rayleigh                           &
     &         (new_sph_mesh, r_itp, ra_rst)
!
      type(sph_mesh_data), intent(in) :: new_sph_mesh
      type(sph_radial_itp_data), intent(in) :: r_itp
!
      type(rayleigh_restart), intent(inout) :: ra_rst
!
      real(kind = kreal), allocatable :: theta_org(:)
!
      integer(kind = kint) :: k1, k2, nmat
      integer(kind = kint) :: k_ICB
      real(kind = kreal) :: r_ICB, r_norm
!
!
      allocate(ra_rst%Cheby_fwd(ra_rst%nri_org,ra_rst%nri_org))
!
      if(my_rank .eq. 0) then
        allocate(theta_org(ra_rst%nri_org))
!
        do k2 = 1, ra_rst%nri_org
          k_ICB = r_itp%kr_inner_domain
          r_ICB = new_sph_mesh%sph%sph_rj%radius_1d_rj_r(k_ICB)
          r_norm = two * (ra_rst%r_org(k2) - r_ICB) - one
          if(r_norm .gt.  one) r_norm =  one
          if(r_norm .lt. -one) r_norm = -one
          theta_org(k2) = acos(r_norm)
        end do
!
        write(*,*) 'k2, theta_org(k2)'
        do k2 = 1, ra_rst%nri_org
          write(*,*) k2, ra_rst%r_org(k2),  theta_org(k2)
        end do
!
        do k1 = 1, ra_rst%nri_org
          do k2 = 1, ra_rst%nri_org
            ra_rst%Cheby_fwd(k2,k1) = cos(dble(k1-1) * theta_org(k2))
          end do
        end do
!
        deallocate(theta_org)
      end if
!
      nmat = ra_rst%nri_org*ra_rst%nri_org
      call MPI_Bcast(ra_rst%Cheby_fwd, nmat,                            &
     &    CALYPSO_REAL, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine chebyshev_fwd_mat_4_rayleigh
!
! -----------------------------------------------------------------------
!
      subroutine check_chebyshev_trans                                  &
     &         (sph_rj, r_itp, file_name, l, m, j, i_fld, i_comp,       &
     &          nri_org, rayleigh_in, nri_tgt, rayleigh_tg, new_sph_phys)
!
      use t_spheric_rj_data
      use t_phys_data
      use r_interpolate_marged_sph
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(sph_radial_itp_data), intent(in) :: r_itp
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: i_fld, i_comp, l, m, j
      integer(kind = kint), intent(in) :: nri_org, nri_tgt
      real(kind = kreal), intent(in) :: rayleigh_in(nri_org,2)
!
      real(kind = kreal), intent(in) :: rayleigh_tg(nri_tgt,1)
      type(phys_data), intent(in) :: new_sph_phys
!
      integer(kind = kint) :: k, kr, inod, iflag
!
!
      iflag = 0
      if(l .eq. 0 .and. m .eq.  0) iflag = 1
      if(l .eq. 2 .and. m .eq.  0) iflag = 1
      if(l .eq. 5 .and. m .eq.  4) iflag = 1
      if(l .eq. 5 .and. m .eq. -4) iflag = 1
      if(iflag .eq. 0) return
!
      write(50+my_rank,*) trim(file_name), l, m
      do k = 1, ra_rst_s%nri_org
        write(50+my_rank,*) k, rayleigh_in(k,1:2)
      end do
!
      write(50+my_rank,*) 'tgt', trim(file_name), l, m, nri_tgt
      do k = 1, nri_tgt
        kr = r_itp%kr_inner_domain + k - 1
        write(50+my_rank,*) k, sph_rj%radius_1d_rj_r(kr),               &
     &                      rayleigh_tg(k,1)
      end do
!
      write(50+my_rank,*) 'fld', trim(file_name), l, m, nri_tgt
!      do k = 1, ra_rst_s%nri_org
!        write(50+my_rank,*) k, rayleigh_fd(k,1)
!      end do
!
      write(50+my_rank,*) trim(new_sph_phys%phys_name(i_fld)), i_comp
      do k = 1, sph_rj%nidx_rj(1)
        inod = j + (k-1) * sph_rj%nidx_rj(2)
        write(50+my_rank,*) k, sph_rj%radius_1d_rj_r(k),                &
     &                      l, m, new_sph_phys%d_fld(inod,i_comp)
      end do
!
      end subroutine check_chebyshev_trans
!
! -----------------------------------------------------------------------
!
      subroutine check_rayleigh_restart_reading                         &
     &         (file_name, i_comp, ra_rst)
!
      use calypso_mpi
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
!
      type(rayleigh_restart), intent(in) :: ra_rst
      character(len = kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: i_comp
!
      type(calypso_MPI_IO_params), save :: IO_param
      integer(kind = MPI_OFFSET_KIND) :: ioffset1, ioffset2
      integer(kind = kint) :: j
      character(len = kchara) :: fn_out
      integer(kind = kint_gl) :: jmax_h
      real(kind = kreal) :: read_fld(2)
!
!
      call open_read_mpi_file                                           &
     &   (file_name, nprocs, my_rank, IO_param)
      if(my_rank .eq. 0) then
        write(fn_out,'(a,i1)') 'rayleigh_test.', i_comp
        open(99,file=fn_out)
!
        jmax_h = 1 + ra_rst%ltr_org*(ra_rst%ltr_org+3) / 2
        do j = 1, int(ra_rst%nri_org * jmax_h)
          ioffset1 = (j-1) * kreal
          ioffset2 = ioffset1 + kreal*ra_rst%nri_org*jmax_h
          call calypso_mpi_seek_read_real                               &
     &       (IO_param%id_file, ra_rst%iflag_swap,                      &
     &        ioffset1, ione, read_fld(1))
          call calypso_mpi_seek_read_real                               &
     &       (IO_param%id_file, ra_rst%iflag_swap,                      &
     &        ioffset2, ione, read_fld(2))
!
          write(99,*) j, read_fld(1:2)
        end do
!
        close(99)
      end if
      call close_mpi_file(IO_param)
      call calypso_mpi_barrier
!
      end subroutine check_rayleigh_restart_reading
!
! -----------------------------------------------------------------------
!
      subroutine check_rayleigh_field_param(id_file, new_sph_phys)
!
      integer(kind = kint), intent(in) :: id_file
      type(phys_data), intent(in) :: new_sph_phys
!
      integer(kind = kint) :: k
!
!
      write(id_file,*) 'num_phys', new_sph_phys%num_phys
      write(id_file,*) 'num_component', new_sph_phys%num_component
      write(id_file,*) 'size', size(new_sph_phys%d_fld,1), &
     &                      size(new_sph_phys%d_fld,2)
      write(id_file,*) 'new_sph_phys%fld_name'
      do k = 1, new_sph_phys%num_phys
        write(id_file,*) k, trim(new_sph_phys%phys_name(k))
      end do
!
      end subroutine check_rayleigh_field_param
!
! -----------------------------------------------------------------------
!
      subroutine chech_field_name_4_IO(id_field, fld_IO)
!
      integer(kind = kint), intent(in) :: id_field
      type(field_IO), intent(in) :: fld_IO
!
      integer(kind = kint) :: k
!
!
      write(id_field,*) 'fld_IO%num_field_IO', fld_IO%num_field_IO
      write(id_field,*) 'fld_IO%num_comp_IO', fld_IO%num_comp_IO
      write(id_field,*) 'fld_IO%fld_name'
      do k = 1, fld_IO%num_field_IO
        write(id_field,*) k, trim(fld_IO%fld_name(k))
      end do
!
      end subroutine chech_field_name_4_IO
!
! -----------------------------------------------------------------------
!
      end  module analyzer_rayleigh_convert
