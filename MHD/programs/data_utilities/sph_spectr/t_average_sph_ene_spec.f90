!>@file   t_average_sph_ene_spec.f90
!!        program t_average_sph_ene_spec
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Evaluate time average and standard deviation 
!!        from spherical harmonics spectrum data
!
      program t_average_sph_ene_spec
!
      use m_precision
!
      use m_tave_sph_ene_spectr
      use m_sph_ene_spectra
      use cal_tave_sph_ene_spectr
!
      implicit none
!
!
      integer(kind = kint) :: ierr, i
      integer(kind = kint) :: icou, istep
      real(kind = kreal) :: start_time, end_time
!
!
      call select_sph_ene_spec_data_file
      call set_org_ene_spec_file_name
!
      write(*,*) 'Input start and end time'
      read(*,*) start_time, end_time
!
!    Evaluate time average
!
      if(iflag_sph_ene_file .eq. 1) then
        call count_degree_on_volume_data
!
        call allocate_sph_sq_data
        call volume_sph_average(fname_org_rms, ave_spec_t)
        call volume_sph_std_deviation(fname_org_rms, ave_spec_t)
        call deallocate_tave_sph_data
        call deallocate_sph_sq_data
!
        call allocate_sph_spectr_data
        call allocate_tave_sph_espec_data
        call volume_spectr_average(fname_org_rms_l,  ave_spec_l)
        call volume_spectr_std_deviation(fname_org_rms_l,  ave_spec_l)
!
        call volume_spectr_average(fname_org_rms_m,  ave_spec_l)
        call volume_spectr_std_deviation(fname_org_rms_m,  ave_spec_l)
!
        call volume_spectr_average(fname_org_rms_lm, ave_spec_l)
        call volume_spectr_std_deviation(fname_org_rms_lm, ave_spec_l)
        call deallocate_tave_sph_espec_data
      else
        if(iflag_sph_ene_file .eq. 2) then
          call count_degree_on_layer_data
        else
          call count_degree_one_layer_data
        end if
!
        call allocate_sph_sq_data
        call layered_sph_average(fname_org_rms, ave_spec_t)
        call layerd_sph_std_deviation(fname_org_rms, ave_spec_t)
        call deallocate_tave_sph_data
        call deallocate_sph_sq_data
!
        call allocate_sph_spectr_data
        call allocate_tave_sph_espec_data
        call layerd_spectr_average(fname_org_rms_l,  ave_spec_l)
        call layerd_spectr_std_deviation(fname_org_rms_l,  ave_spec_l)
!
        call layerd_spectr_average(fname_org_rms_m,  ave_spec_l)
        call layerd_spectr_std_deviation(fname_org_rms_m,  ave_spec_l)
!
        call layerd_spectr_average(fname_org_rms_lm, ave_spec_l)
        call layerd_spectr_std_deviation(fname_org_rms_lm, ave_spec_l)
        call deallocate_tave_sph_espec_data
      end if
!
      stop
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine volume_sph_average(fname_org, ave_spec_t)
!
      use m_sph_ene_spectra
!
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(inout)                                 &
     &             :: ave_spec_t(ncomp_sph_spec,ione)
!
      character(len = kchara) :: file_name
!
!  Evaluate standard deviation
!
!
      open(id_file_rms,   file=fname_org)
      call read_ene_spectr_header(id_file_rms, ione, ene_sph_spec_name)
!
      icou = 0
      ist_true = -1
      pre_time = time_sph
      ave_spec_t =  0.0d0
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', istep,   ' averaging finished. Count=  ', icou
      do
        if(read_vol_ene_sph(id_file_rms, istep, time_sph,               &
     &      ncomp_sph_spec, spectr_t(1,1)) .gt. 0) go to 99
!
        if (time_sph .ge. start_time) then
          if (ist_true .eq. -1) ist_true = istep
          icou = icou + 1
!
          call sum_average_ene_sph(time_sph, pre_time, nri_sph,         &
     &        ncomp_sph_spec, spectr_t, ave_spec_t, spectr_pre_t)
          pre_time = time_sph
!
          if (icou .eq. 1) time_ini = time_sph
        end if
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', istep,   ' averaging finished. Count=   ', icou
        if (time_sph .ge. end_time) exit
      end do
!
   99 continue
      write(*,*)
      close(id_file_rms)
!
!
      write(*,*) '(time_sph - time_ini)', (time_sph - time_ini)
      call divide_average_ene_sph                                       &
     &   (time_sph, time_ini, nri_sph, ncomp_sph_spec, ave_spec_t)
!
      write(file_name,   '(a6,a)') 't_ave_', trim(fname_org)
      call write_tave_vol_sph_data(file_name, ene_sph_spec_name,        &
     &    istep, time_sph, ncomp_sph_spec, ave_spec_t(1,1))
!
      end subroutine volume_sph_average
!
!   --------------------------------------------------------------------
!
      subroutine layered_sph_average(fname_org, ave_spec_t)
!
      use m_sph_ene_spectra
!
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(inout)                                 &
     &             :: ave_spec_t(ncomp_sph_spec,nri_sph)
!
      character(len = kchara) :: file_name
!
!  Evaluate standard deviation
!
      open(id_file_rms,   file=fname_org)
      call read_ene_spectr_header(id_file_rms, ione, ene_sph_spec_name)
!
      icou = 0
      ist_true = -1
      pre_time = time_sph
      ave_spec_t =  0.0d0
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', istep,   ' averaging finished. Count=  ', icou
      do
        if(read_layer_ene_sph(id_file_rms, istep, time_sph,             &
     &      nri_sph, ncomp_sph_spec, spectr_t) .gt. 0) go to 99
!
        if (time_sph .ge. start_time) then
          if (ist_true .eq. -1) ist_true = istep
          icou = icou + 1
!
          call sum_average_ene_sph(time_sph, pre_time, nri_sph,         &
     &        ncomp_sph_spec, spectr_t, ave_spec_t, spectr_pre_t)
          pre_time = time_sph
!
          if (icou .eq. 1) time_ini = time_sph
        end if
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', istep,   ' averaging finished. Count=   ', icou
        if (time_sph .ge. end_time) exit
      end do
!
   99 continue
      write(*,*)
      close(id_file_rms)
!
!
      write(*,*) '(time_sph - time_ini)', (time_sph - time_ini)
      call divide_average_ene_sph                                       &
     &   (time_sph, time_ini, nri_sph, ncomp_sph_spec, ave_spec_t)
!
      write(file_name,   '(a6,a)') 't_ave_', trim(fname_org)
      call write_tave_layer_sph_data(file_name, ene_sph_spec_name,      &
     &    istep, time_sph, nri_sph, ncomp_sph_spec, ave_spec_t)
!
      end subroutine layered_sph_average
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine volume_sph_std_deviation(fname_org, ave_spec_t)
!
      use m_sph_ene_spectra
!
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(inout)                                 &
     &             :: ave_spec_t(ncomp_sph_spec,nri_sph)
!
      character(len = kchara) :: file_name
!
!  Evaluate standard deviation
!
!
      open(id_file_rms,   file=fname_org)
      call read_ene_spectr_header(id_file_rms,   ione, ene_sph_spec_name)
!
      icou = 0
      ist_true = -1
      pre_time = time_sph
      sigma_spec_t =  0.0d0
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', istep,   ' deviation finished. Count=  ', icou
      do
        if(read_vol_ene_sph(id_file_rms, istep, time_sph,               &
     &     ncomp_sph_spec, spectr_t(1,1)) .gt. 0) go to 99
!
        if (time_sph .ge. start_time) then
          if (ist_true .eq. -1) ist_true = istep
          icou = icou + 1
!
          call sum_deviation_ene_sph(time_sph, pre_time,                &
     &      ione, ncomp_sph_spec, spectr_t, ave_spec_t,                 &
     &      sigma_spec_t, spectr_pre_t)
!
          pre_time = time_sph
!
          if (icou .eq. 1) time_ini = time_sph
        end if
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', istep,   ' deviation finished. Count=   ', icou
        if (time_sph .ge. end_time) exit
      end do
   99 continue
      write(*,*)
      close(id_file_rms)
!
      call divide_deviation_ene_sph(time_sph, time_ini,                 &
     &    ione, ncomp_sph_spec, sigma_spec_t)
!
      write(file_name,   '(a8,a)') 't_sigma_', trim(fname_org)
      call write_tave_vol_sph_data(file_name, ene_sph_spec_name,        &
     &    istep, time_sph, ncomp_sph_spec, sigma_spec_t(1,1))
!
      end subroutine volume_sph_std_deviation
!
!   --------------------------------------------------------------------
!
      subroutine layerd_sph_std_deviation(fname_org, ave_spec_t)
!
      use m_sph_ene_spectra
!
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(inout)                                 &
     &             :: ave_spec_t(ncomp_sph_spec,nri_sph)
!
      character(len = kchara) :: file_name
!
!  Evaluate standard deviation
!
!
      open(id_file_rms,   file=fname_org)
      call read_ene_spectr_header(id_file_rms,   ione, ene_sph_spec_name)
!
      icou = 0
      ist_true = -1
      pre_time = time_sph
      sigma_spec_t =  0.0d0
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', istep,   ' deviation finished. Count=  ', icou
      do
        if(read_layer_ene_sph(id_file_rms, istep, time_sph,             &
     &     nri_sph, ncomp_sph_spec, spectr_t) .gt. 0) go to 99
!
        if (time_sph .ge. start_time) then
          if (ist_true .eq. -1) ist_true = istep
          icou = icou + 1
!
          call sum_deviation_ene_sph(time_sph, pre_time,                &
     &      nri_sph, ncomp_sph_spec, spectr_t, ave_spec_t,              &
     &      sigma_spec_t, spectr_pre_t)
!
          pre_time = time_sph
!
          if (icou .eq. 1) time_ini = time_sph
        end if
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', istep,   ' deviation finished. Count=   ', icou
        if (time_sph .ge. end_time) exit
      end do
   99 continue
      write(*,*)
      close(id_file_rms)
!
!
!
      call divide_deviation_ene_sph(time_sph, time_ini,                 &
     &    nri_sph, ncomp_sph_spec, sigma_spec_t)
!
      write(file_name,   '(a8,a)') 't_sigma_', trim(fname_org)
      call write_tave_layer_sph_data(file_name, ene_sph_spec_name,      &
     &    istep, time_sph, nri_sph, ncomp_sph_spec, sigma_spec_t)
!
!
      end subroutine layerd_sph_std_deviation
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine volume_spectr_average(fname_org, ave_spec_l)
!
      use m_sph_ene_spectra
!
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(inout)                                 &
     &             :: ave_spec_l(ncomp_sph_spec,0:ltr_sph,ione)
!
      character(len = kchara) :: file_name
!
!
      open(id_file_rms_l, file=fname_org)
      call read_ene_spectr_header                                       &
     &   (id_file_rms_l, izero, ene_sph_spec_name)
!
      icou = 0
      ist_true = -1
      pre_time = time_sph
      ave_spec_l =  0.0d0
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', istep,   ' averaging finished. Count=  ', icou
      do
        if(read_layer_ene_spectr(id_file_rms_l, istep, time_sph,        &
     &     nri_sph, ltr_sph, ncomp_sph_spec, spectr_l) .gt. 0) go to 99
!
        if (time_sph .ge. start_time) then
          if (ist_true .eq. -1) ist_true = istep
          icou = icou + 1
!
          call sum_average_ene_spectr                                   &
     &       (time_sph, pre_time, nri_sph, ltr_sph, ncomp_sph_spec,     &
     &        spectr_l, ave_spec_l, spectr_pre_l)
          pre_time = time_sph
!
          if (icou .eq. 1) time_ini = time_sph
        end if
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', istep,   ' averaging finished. Count=   ', icou
        if (time_sph .ge. end_time) exit
      end do
!
   99 continue
      write(*,*)
      close(id_file_rms_l)
!
!
      call divide_average_ene_spectr(time_sph, time_ini,                &
     &    nri_sph, ltr_sph, ncomp_sph_spec, ave_spec_l)
!
      write(file_name, '(a6,a)') 't_ave_', trim(fname_org)
      call write_tave_vol_spectr_data(file_name, ene_sph_spec_name,     &
     &    istep, time_sph, ltr_sph, ncomp_sph_spec,                     &
     &    ave_spec_l(1,0,1))
!
      end subroutine volume_spectr_average
!
!   --------------------------------------------------------------------
!
      subroutine layerd_spectr_average(fname_org, ave_spec_l)
!
      use m_sph_ene_spectra
!
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(inout)                                 &
     &             :: ave_spec_l(ncomp_sph_spec,0:ltr_sph,nri_sph)
!
      character(len = kchara) :: file_name
!
!
      open(id_file_rms_l, file=fname_org)
      call read_ene_spectr_header                                       &
     &   (id_file_rms_l, izero, ene_sph_spec_name)
!
      icou = 0
      ist_true = -1
      pre_time = time_sph
      ave_spec_l =  0.0d0
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', istep,   ' averaging finished. Count=  ', icou
      do
        if(read_layer_ene_spectr(id_file_rms_l, istep, time_sph,        &
     &     nri_sph, ltr_sph, ncomp_sph_spec, spectr_l) .gt. 0) go to 99
!
        if (time_sph .ge. start_time) then
          if (ist_true .eq. -1) ist_true = istep
          icou = icou + 1
!
          call sum_average_ene_spectr                                   &
     &       (time_sph, pre_time, nri_sph, ltr_sph, ncomp_sph_spec,     &
     &        spectr_l, ave_spec_l, spectr_pre_l)

          pre_time = time_sph
          if (icou .eq. 1) time_ini = time_sph
        end if
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', istep,   ' averaging finished. Count=   ', icou
        if (time_sph .ge. end_time) exit
      end do
!
   99 continue
      write(*,*)
      close(id_file_rms_l)
!
!
      call divide_average_ene_spectr(time_sph, time_ini,                &
     &    nri_sph, ltr_sph, ncomp_sph_spec, ave_spec_l)
!
      write(file_name, '(a6,a)') 't_ave_', trim(fname_org)
      call write_tave_layer_spectr_data(file_name, ene_sph_spec_name,   &
     &    istep, time_sph, nri_sph, ltr_sph,                            &
     &    ncomp_sph_spec, ave_spec_l)
!
      end subroutine layerd_spectr_average
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      subroutine volume_spectr_std_deviation(fname_org, ave_spec_l)
!
      use m_sph_ene_spectra
!
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(in)                                    &
     &             :: ave_spec_l(ncomp_sph_spec,0:ltr_sph,ione)
!
      character(len = kchara) :: file_name
!
!  Evaluate standard deviation
!
      open(id_file_rms, file=fname_org)
      call read_ene_spectr_header(id_file_rms, ione, ene_sph_spec_name)
!
      icou = 0
      ist_true = -1
      pre_time = time_sph
      sigma_spec_l =  0.0d0
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', istep,   ' deviation finished. Count=  ', icou
      do
        if(read_vol_ene_spectr(id_file_rms, istep, time_sph,            &
     &      ltr_sph, ncomp_sph_spec, spectr_l(1,0,1)) .gt. 0) go to 99
!
        if (time_sph .ge. start_time) then
          if (ist_true .eq. -1) ist_true = istep
          icou = icou + 1
!
          call sum_deviation_ene_spectr(time_sph, pre_time,             &
     &        ione, ltr_sph, ncomp_sph_spec, spectr_l, ave_spec_l,      &
     &        sigma_spec_l, spectr_pre_l)
!
          pre_time = time_sph
          if (icou .eq. 1)  time_ini = time_sph
        end if
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', istep,   ' deviation finished. Count=   ', icou
        if (time_sph .ge. end_time) exit
      end do
   99 continue
      write(*,*)
      close(id_file_rms)
!
      call divide_deviation_ene_spectr(time_sph, time_ini,              &
     &    ione, ltr_sph, ncomp_sph_spec, sigma_spec_l)
!
      write(file_name, '(a8,a)') 't_sigma_', trim(fname_org)
      call write_tave_vol_spectr_data(file_name, ene_sph_spec_name,     &
     &    istep, time_sph, ltr_sph, ncomp_sph_spec,                     &
     &    sigma_spec_l(1,0,1))
!
      end subroutine volume_spectr_std_deviation
!
!   --------------------------------------------------------------------
!
      subroutine layerd_spectr_std_deviation(fname_org, ave_spec_l)
!
      use m_sph_ene_spectra
!
      character(len = kchara), intent(in) :: fname_org
      real(kind = kreal), intent(in)                                    &
     &             :: ave_spec_l(ncomp_sph_spec,0:ltr_sph,nri_sph)
!
      character(len = kchara) :: file_name
!
!  Evaluate standard deviation
!
      open(id_file_rms, file=fname_org)
      call read_ene_spectr_header(id_file_rms, izero, ene_sph_spec_name)
!
      icou = 0
      ist_true = -1
      pre_time = time_sph
      sigma_spec_l =  0.0d0
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', istep,   ' deviation finished. Count=  ', icou
      do
        if(read_layer_ene_spectr(id_file_rms, istep, time_sph,          &
     &     nri_sph, ltr_sph, ncomp_sph_spec, spectr_l) .gt. 0) go to 99
!
        if (time_sph .ge. start_time) then
          if (ist_true .eq. -1) ist_true = istep
          icou = icou + 1
!
          call sum_deviation_ene_spectr(time_sph, pre_time,             &
     &        nri_sph, ltr_sph, ncomp_sph_spec, spectr_l, ave_spec_l,   &
     &        sigma_spec_l, spectr_pre_l)
!
          pre_time = time_sph
          if (icou .eq. 1) time_ini = time_sph
        end if
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', istep,   ' deviation finished. Count=   ', icou
        if (time_sph .ge. end_time) exit
      end do
   99 continue
      write(*,*)
      close(id_file_rms)
!
      call divide_deviation_ene_spectr(time_sph, time_ini,              &
     &    nri_sph, ltr_sph, ncomp_sph_spec, sigma_spec_l)
!
      write(file_name, '(a8,a)') 't_sigma_', trim(fname_org)
      call write_tave_layer_spectr_data(file_name, ene_sph_spec_name,   &
     &    istep, time_sph,  nri_sph, ltr_sph,                           &
     &    ncomp_sph_spec, sigma_spec_l)
!
      end subroutine layerd_spectr_std_deviation
!
!   --------------------------------------------------------------------
!
      end program t_average_sph_ene_spec
