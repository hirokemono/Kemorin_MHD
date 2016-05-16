!neutral_point_by_pick_sph.f90
!      program neutral_point_by_pick_sph
!
!        programmed by H.Matsui on Dec., 2012
!
      program neutral_point_by_pick_sph
!
      use m_precision
      use m_constants
      use m_spheric_parameter
      use m_pickup_sph_spectr_data
      use m_neutral_pt_by_pick_sph
!
      implicit  none
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
      pickup_sph_head = evo_header
!
      write(*,*) 'input start, end, increment steps'
      read(5,*) istep_start, istep_end, istep_inc
!
      call open_sph_spec_read_monitor(id_pick)
      call find_field_address
!
      call read_sph_spec_4_monitor(id_pick, i_step, time, ierr)
      icou = 0
!
      call set_radius_for_fdm(sph_param1, sph_rj1)
      call alloc_neutral_point
!
      do
        if(mod((i_step-istep_start),istep_inc) .eq. 0                   &
     &     .and. i_step.ge.istep_start) then
!
          call set_radial_grad_scalars(i_step, time,                    &
     &        sph_rj1%nidx_rj(1), sph_rj1%radius_1d_rj_r,               &
     &        buoyancy_ratio)
          icou = icou + 1
          write(*,*) 'step ', i_step,                                   &
     &        ' is added for time average: count is  ', icou
        end if
!
        if(i_step .ge. istep_end) exit
!
        call read_sph_spec_4_monitor(id_pick, i_step, time, ierr)
        if(ierr .gt. 0) exit
      end do
!
      call dealloc_neutral_point
      close(id_pick)
!
      write(*,*) '***** program finished *****'
      stop
      end program neutral_point_by_pick_sph
!
