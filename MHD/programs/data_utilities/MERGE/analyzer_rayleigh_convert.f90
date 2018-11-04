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
      integer(kind = kint) :: ip, jp, k
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
        if(my_rank .eq. 0) then
          call read_rayleigh_restart_params                             &
     &       (asbl_param_s%org_fld_file%file_prefix,                    &
     &        asbl_param_s%istep_start, ra_rst_s)
!
          sph_asbl_s%org_sph_mesh(1)%sph%sph_rj%nidx_rj(1) = ra_rst_s%nri_org
          sph_asbl_s%org_sph_mesh(1)%sph%sph_rj%nidx_rj(2) = 1
          call alloc_type_sph_1d_index_rj(sph_asbl_s%org_sph_mesh(1)%sph%sph_rj)
          do k = 1, sph_asbl_s%org_sph_mesh(1)%sph%sph_rj%nidx_rj(1)
            sph_asbl_s%org_sph_mesh(1)%sph%sph_rj%radius_1d_rj_r(k)      &
     &         = ra_rst_s%r_org(ra_rst_s%nri_org-k+1)
          end do
        end if
        call bcast_rayleigh_rst_params(ra_rst_s)
        call check_rayleigh_rst_params(50+my_rank, ra_rst_s)
        call dealloc_rayleigh_radial_grid(ra_rst_s)
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
!      Construct field list from spectr file
!
      call init_rayleigh_restart_input                                  &
     &     (asbl_param_s%org_fld_file%file_prefix,                      &
     &      asbl_param_s%istep_start, fld_IO_r)
!
        write(50+my_rank,*) 'fld_IO_r%num_field_IO', fld_IO_r%num_field_IO
        write(50+my_rank,*) 'fld_IO_r%num_comp_IO', fld_IO_r%num_comp_IO
        write(50+my_rank,*) 'fld_IO_r%fld_name'
        do k = 1, fld_IO_r%num_field_IO
          write(50+my_rank,*) k, trim(fld_IO_r%fld_name(k))
        end do
!
      if(my_rank .eq. 0) then
        call copy_rj_phys_name_from_IO                                  &
     &     (fld_IO_r, sph_asbl_s%new_sph_phys(1))
      end if
      call share_new_spectr_field_names(sph_asbl_s%np_sph_new,          &
     &    sph_asbl_s%new_sph_mesh, sph_asbl_s%new_sph_phys(1))
!
      write(50+my_rank,*) 'num_phys', sph_asbl_s%new_sph_phys(1)%num_phys
      write(50+my_rank,*) 'num_component', sph_asbl_s%new_sph_phys(1)%num_component
      write(50+my_rank,*) 'size', size(sph_asbl_s%new_sph_phys(1)%d_fld,1), &
     &                      size(sph_asbl_s%new_sph_phys(1)%d_fld,2)
      write(50+my_rank,*) 'new_sph_phys%fld_name'
      do k = 1, sph_asbl_s%new_sph_phys(1)%num_phys
        write(50+my_rank,*) k, trim(sph_asbl_s%new_sph_phys(1)%phys_name(k))
      end do
!
      end subroutine init_cvt_rayleigh
!
! ----------------------------------------------------------------------
!
      subroutine analyze_cvt_rayleigh
!
      use m_phys_labels
      use r_interpolate_marged_sph
      use set_field_file_names
      use share_field_data
!
      use m_calypso_mpi_IO
      use t_calypso_mpi_IO_param
      use MPI_ascii_data_IO
!
      integer(kind = kint) :: istep, icou
      integer(kind = kint) :: ip, jp, irank_new
      integer(kind = kint) :: iloop, jloop
      integer(kind = kint) :: istep_out
!
      integer(kind = MPI_OFFSET_KIND) :: ioffset1, ioffset2
      integer(kind = kint) :: k, kr, l, m, j, iflag
      integer(kind = kint) :: i_fld, i_comp, nd
      integer(kind = kint) :: iflag_ncomp
      character(len = kchara) :: file_name(2)
      real(kind = kreal) :: rayleigh_in(2)
      real(kind = kreal), allocatable :: rayleigh_r(:,:)
      type(calypso_MPI_IO_params), save :: IO_param
!
      integer(kind = kint) :: inod, k_in, k_out
      real(kind = kreal) :: coef
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
        allocate(rayleigh_r(ra_rst_s%nri_org,2))
        rayleigh_r = 0.0d0
!
        do i_fld = 1, sph_asbl_s%new_sph_phys(1)%num_phys
          i_comp = sph_asbl_s%new_sph_phys(1)%istack_component(i_fld-1)
!          if(my_rank .eq. 0) write(*,*) 'tako', i_fld, i_comp,         &
!     &         trim(sph_asbl_s%new_sph_phys(1)%phys_name(i_fld))
          call set_rayleigh_rst_file_name                               &
     &       (asbl_param_s%org_fld_file%file_prefix, istep,             &
     &        sph_asbl_s%new_sph_phys(1)%phys_name(i_fld),              &
     &        iflag_ncomp, file_name(1))
!
          do nd = 1, iflag_ncomp
            if(my_rank .eq. 0) write(*,*) i_fld, iflag_ncomp, trim(file_name(nd))
            call open_read_mpi_file                                     &
     &         (file_name(nd), nprocs, my_rank, IO_param)
!
            write(50+my_rank,*) 'k, kr, l, m, ioffset1, ioffset2', &
       &     sph_asbl_s%new_sph_mesh(my_rank+1)%sph%sph_rj%nidx_rj(1:2)
            do j = 1, sph_asbl_s%new_sph_mesh(my_rank+1)%sph%sph_rj%nidx_rj(2)
              l = sph_asbl_s%new_sph_mesh(my_rank+1)%sph%sph_rj%idx_gl_1d_rj_j(j,2)
              m = sph_asbl_s%new_sph_mesh(my_rank+1)%sph%sph_rj%idx_gl_1d_rj_j(j,3)
              if(l .gt. ra_rst_s%ltr_org) cycle
              do k = 1, ra_rst_s%nri_org
                call find_rayleigh_restart_address                      &
     &             (ra_rst_s%nri_org, ra_rst_s%ltr_org,                 &
     &              k, l, abs(m), ioffset1, ioffset2)
!
                call calypso_mpi_seek_read_real                         &
     &             (IO_param%id_file, ra_rst_s%iflag_swap,              &
     &              ioffset1, ione, rayleigh_in(1))
                call calypso_mpi_seek_read_real                         &
     &             (IO_param%id_file, ra_rst_s%iflag_swap,              &
     &              ioffset2, ione, rayleigh_in(2))
!
               kr = ra_rst_s%nri_org - k + 1
               rayleigh_r(kr,1) = rayleigh_in(1)
               rayleigh_r(kr,2) = rayleigh_in(2)
              end do
!
              do k = 1, sph_asbl_s%new_sph_mesh(my_rank+1)%sph%sph_rj%nidx_rj(1)
                inod = j + (k-1) * sph_asbl_s%new_sph_mesh(my_rank+1)%sph%sph_rj%nidx_rj(2)
!
                if     (k .lt. sph_asbl_s%r_itp%kr_inner_domain) then
                  sph_asbl_s%new_sph_phys(my_rank+1)%d_fld(inod,nd  ) = 0.0d0
                  sph_asbl_s%new_sph_phys(my_rank+1)%d_fld(inod,nd+1) = 0.0d0
                else if(k .gt. sph_asbl_s%r_itp%kr_outer_domain) then
                  sph_asbl_s%new_sph_phys(my_rank+1)%d_fld(inod,nd  ) = 0.0d0
                  sph_asbl_s%new_sph_phys(my_rank+1)%d_fld(inod,nd+1) = 0.0d0
                else
                  k_in =  ra_rst_s%nri_org - sph_asbl_s%r_itp%k_old2new_in(k) +  1
                  k_out = ra_rst_s%nri_org - sph_asbl_s%r_itp%k_old2new_out(k) + 1
                  coef = sph_asbl_s%r_itp%coef_old2new_in(k)
                  sph_asbl_s%new_sph_phys(my_rank+1)%d_fld(inod,i_comp+nd) = coef * rayleigh_r(k_in,1)           &
     &                           + (1.0d0 - coef) * rayleigh_r(k_out,1) 
!                  sph_asbl_s%new_sph_phys(my_rank+1)%d_fld(inod,nd+1) = coef * rayleigh_r(k_in,2)           &
!     &                           + (1.0d0 - coef) * rayleigh_r(k_out,2) 
                end if
              end do
!
!              do k = 1, ra_rst_s%nri_org
!                iflag = 0
!                if(l .eq. 1 .and. m .eq.  0) iflag = 1
!                if(l .eq. 4 .and. m .eq.  4) iflag = 1
!                if(l .eq. 4 .and. m .eq. -4) iflag = 1
!                if(iflag .eq. 1) then
!                   kr = ra_rst_s%nri_org - k + 1
!                  write(50+my_rank,*) k, kr, l, m, rayleigh_r(k,1:2)
!                end if
!              end do
!
              do k = 1, ra_rst_s%nri_org
                iflag = 0
                if(l .eq. 1 .and. m .eq.  0) iflag = 1
                if(l .eq. 4 .and. m .eq.  4) iflag = 1
                if(l .eq. 4 .and. m .eq. -4) iflag = 1
                if(iflag .eq. 1) then
                  inod = j + (k-1) * sph_asbl_s%new_sph_mesh(my_rank+1)%sph%sph_rj%nidx_rj(2)
                   kr = ra_rst_s%nri_org - k + 1
                  write(50+my_rank,*) k, l, m, sph_asbl_s%new_sph_phys(my_rank+1)%d_fld(inod,i_comp:i_comp+iflag_ncomp)
                end if
              end do
             end do
!
            call close_mpi_file(IO_param)
          end do
!
        end do
        deallocate(rayleigh_r)
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
      end module analyzer_rayleigh_convert
