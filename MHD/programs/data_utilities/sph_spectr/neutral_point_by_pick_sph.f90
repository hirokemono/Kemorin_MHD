!neutral_point_by_pick_sph.f90
!      program neutral_point_by_pick_sph
!
!        programmed by H.Matsui on Dec., 2012
!
      program neutral_point_by_pick_sph
!
      use m_precision
      use m_constants
      use m_spheric_data_sph_spetr
      use t_neutral_pt_by_pick_sph
      use t_fdm_coefs
      use t_picked_sph_spectr_data_IO
      use picked_sph_spectr_data_IO
      use set_parallel_file_name
!
      implicit  none
!
      type(fdm_matrices), save :: r_2nd_newtral
      type(picked_spectrum_data_IO), save :: pick_IO
      type(neutral_pt_by_pick_sph), save :: ntl1
!
      character(len=kchara) :: evo_header, file_name
      real(kind = kreal) :: buoyancy_ratio
!
      real(kind = kreal) :: start_time, end_time
      real(kind = kreal) :: true_start, true_end
      integer(kind = kint) :: i_step, icou
      real(kind = kreal) :: time
!
!
      write(*,*) 'input picked spectr evolution file header'
      read(5,*) evo_header
!
      write(*,*) 'input buoyancy ratio'
      read(5,*) buoyancy_ratio
!
      write(*,*) 'input start and end time'
      read(5,*) start_time, end_time
!
      file_name = add_dat_extension(evo_header)
!      Load picked mode file
      call load_picked_sph_spectr_series                                &
     &   (.TRUE., file_name, start_time, end_time,                      &
     &    true_start, true_end, pick_IO)
!
      call find_field_address(pick_IO, ntl1)
      call set_radius_for_fdm(pick_IO,                                  &
     &    SPH_dat_ss%sph%sph_params, SPH_dat_ss%sph%sph_rj,             &
     &    r_2nd_newtral, ntl1)
!
      do icou = 1, pick_IO%n_step
          call set_radial_grad_scalars(icou, i_step, time,              &
     &        SPH_dat_ss%sph%sph_rj%nidx_rj(1),                         &
     &        SPH_dat_ss%sph%sph_rj%radius_1d_rj_r,                     &
     &        r_2nd_newtral%fdm(1)%dmat, buoyancy_ratio, pick_IO, ntl1)
      end do
!
      call dealloc_neutral_point(ntl1)
!
      write(*,*) '***** program finished *****'
      end program neutral_point_by_pick_sph
!
