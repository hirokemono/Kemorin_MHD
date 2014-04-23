!>@file   maxmode_sph_ene_spec.f90
!!        program maxmode_sph_ene_spec
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!> @brief Find maximum degree and order of the spectrum
!
      program maxmode_sph_ene_spec
!
      use m_precision
!
      use m_sph_ene_spectra
      use m_max_sph_ene_spectr
!
      implicit none
!
!
      integer(kind = kint) :: ist, ied, ierr
      integer(kind = kint) :: icou, istep
!
!
      call select_sph_ene_spec_data_file
      call set_org_ene_spec_file_name
!
      write(*,*) 'imput start and end step number'
      read(*,*) ist, ied
!
      if(iflag_sph_ene_file .eq. 1) then
        call count_degree_on_volume_data
      else  if(iflag_sph_ene_file .eq. 2) then
        call count_degree_on_layer_data
      else
        call count_degree_one_layer_data
      end if
      call allocate_sph_espec_data
      call allocate_max_sph_espec_data
!
!    Evaluate time average
!
      call open_org_ene_spec_data
      call open_maxmode_spec_data
!
      ist_true = -1
      icou = 0
      do
        if(iflag_sph_ene_file .eq. 1) then
          call read_org_volume_ene_data(istep, ierr)
        else
          call read_org_layer_ene_data(istep, ierr)
        end if
        if(ierr.gt.0) go to 99
!
        if (istep .ge. ist) then
          if (ist_true .eq. -1) then
            ist_true = istep
          end if
          icou = icou + 1
          ied_true = istep
!
          call find_dominant_scale_sph
          call output_dominant_scale_sph
        end if
!
        if (istep .ge. ied) exit
!
        write(*,*) 'step', istep, 'averagind finished. Count: ', icou
      end do
   99 continue
!
      call close_maxmode_spec_data
      call close_ene_spec_data
!
      stop
      end program maxmode_sph_ene_spec
