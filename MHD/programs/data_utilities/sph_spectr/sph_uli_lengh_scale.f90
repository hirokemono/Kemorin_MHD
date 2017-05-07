!>@file   sph_uli_lengh_scale.f90
!!        program sph_uli_lengh_scale
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Find maximum degree and order of the spectrum
!
      program sph_uli_lengh_scale
!
      use m_precision
!
      use m_sph_ene_spectra
      use m_uli_sph_lscale
!
      implicit none
!
!
      character(len = kchara) :: input_header
      integer(kind = kint) :: ist, ied, ierr
      integer(kind = kint) :: icou, istep, i
!
!
      call select_sph_ene_spec_data_file                                &
     &   (iflag_volume_average, iflag_old_file_format, input_header)
      call set_org_ene_spec_file_name(input_header)
!
      write(*,*) 'imput start and end step number'
      read(*,*) ist, ied
!
      if(iflag_volume_average .eq. 1) then
        open (id_file_rms_l,file=fname_org_rms_l)
        call count_degree_on_volume_spectr(id_file_rms_l)
        close(id_file_rms_l)
      else  if(iflag_volume_average .eq. 0) then
        open (id_file_rms_l,file=fname_org_rms_l)
        call count_degree_on_layer_spectr(id_file_rms_l)
        close(id_file_rms_l)
      end if
      call allocate_sph_espec_data
      call allocate_lscale_espec_data
!
!    Evaluate time average
!
      open(id_file_rms_l, file=fname_org_rms_l)
      call read_ene_spectr_header(id_file_rms_l)
      call open_uli_sph_lscale
!
      ist_true = -1
      icou = 0
      write(*,*)
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', istep,   ' evaluation finished. Count=  ', icou
      do
        if(iflag_volume_average .eq. 1) then
          if(read_vol_ene_spectr(id_file_rms_l, istep, time_sph,        &
     &      ltr_sph, ntot_sph_spec, spectr_l(1,0,1)) .gt. 0) go to 99
        else
          if(read_layer_ene_spectr(id_file_rms_l, istep, time_sph,      &
     &      nri_sph, ltr_sph, ntot_sph_spec, spectr_l) .gt. 0) go to 99
        end if
!
        if (istep .ge. ist) then
          if (ist_true .eq. -1) then
            ist_true = istep
          else
          end if
          icou = icou + 1
          ied_true = istep
!
          call cal_uli_length_scale_sph
          call output_uli_sph_lscale(istep)
        end if
!
        if (istep .ge. ied) exit
!
        write(*,'(59a1,a5,i12,a30,i12)',advance="NO") (char(8),i=1,59), &
     &       'step= ', istep,   ' evaluation finished. Count=  ', icou
      end do
   99 continue
      write(*,*)
!
      call close_uli_sph_lscale
      close(id_file_rms_l)
!
      stop
      end program sph_uli_lengh_scale
