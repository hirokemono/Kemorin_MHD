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
      integer(kind = kint) :: ist, ied, ierr
      integer(kind = kint) :: icou, istep, i
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
      call allocate_lscale_espec_data
!
!    Evaluate time average
!
      call open_org_ene_spec_data
      call open_uli_sph_lscale
!
      ist_true = -1
      icou = 0
      write(*,*)
      write(*,'(a5,i12,a30,i12)',advance="NO")                          &
     &       'step= ', istep,   ' evaluation finished. Count=  ', icou
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
      call close_ene_spec_data
!
      stop
      end program sph_uli_lengh_scale
