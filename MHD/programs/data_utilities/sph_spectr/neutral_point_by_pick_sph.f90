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
!
      implicit  none
!
      type(fdm_matrices), save :: r_2nd_newtral
      type(picked_spectrum_data_IO), save :: pick_IO
      type(neutral_pt_by_pick_sph), save :: ntl1
!
      character(len=kchara) :: evo_header
      real(kind = kreal) :: buoyancy_ratio
!
      integer(kind = kint), parameter :: id_pick = 15
!
      integer(kind = kint) :: istep_start, istep_end, istep_inc
      integer(kind = kint) :: i_step, ierr, icou
      real(kind = kreal) :: time
!
!
      write(*,*) 'input picked spectr evolution file header'
      read(5,*) evo_header
!
      write(*,*) 'input buoyancy ratio'
      read(5,*) buoyancy_ratio
!
      write(*,*) 'input start, end, increment steps'
      read(5,*) istep_start, istep_end, istep_inc
!
      call open_sph_spec_read(id_pick, evo_header, pick_IO)
      call find_field_address(pick_IO, ntl1)
!
      call read_sph_spec_monitor                                        &
     &   (id_pick, i_step, time, pick_IO, ierr)
      icou = 0
!
      call set_radius_for_fdm(pick_IO,                                  &
     &    SPH_dat_ss%sph%sph_params, SPH_dat_ss%sph%sph_rj,             &
     &    r_2nd_newtral, ntl1)
!
      do
        if(mod((i_step-istep_start),istep_inc) .eq. 0                   &
     &     .and. i_step.ge.istep_start) then
!
          call set_radial_grad_scalars(i_step, time,                    &
     &        SPH_dat_ss%sph%sph_rj%nidx_rj(1),                         &
     &        SPH_dat_ss%sph%sph_rj%radius_1d_rj_r,                     &
     &        r_2nd_newtral%fdm(1)%dmat, buoyancy_ratio, pick_IO, ntl1)
          icou = icou + 1
          write(*,*) 'step ', i_step,                                   &
     &        ' is written for neutral points: count is  ', icou
        end if
!
        if(i_step .ge. istep_end) exit
!
        call read_sph_spec_monitor                                      &
     &     (id_pick, i_step, time, pick_IO, ierr)
        if(ierr .gt. 0) exit
      end do
!
      call dealloc_neutral_point(ntl1)
      close(id_pick)
!
      write(*,*) '***** program finished *****'
      stop
      end program neutral_point_by_pick_sph
!
