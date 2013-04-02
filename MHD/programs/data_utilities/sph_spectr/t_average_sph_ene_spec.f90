!t_average_sph_ene_spec.f90
!      program t_average_sph_ene_spec
!
      program t_average_sph_ene_spec
!
!      Written by H. Matsui on Nov., 2007
!
      use m_precision
!
      use m_tave_sph_ene_spectr
      use cal_tave_sph_ene_spectr
!
      implicit none
!
!
      integer(kind = kint) :: ist, ied, ierr
      integer(kind = kint) :: icou, istep
!
!
      write(*,*) ' Choose data type to taking average'
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
      call allocate_tave_sph_espec_data
!
      call open_org_ene_spec_data
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
          call sum_average_ene_sph
!
        end if
!
        if (istep .ge. ied) exit
!
        write(*,*) 'step', istep, ' finished. Count: ', icou
!
      end do
      99 continue
!
      call close_ene_spec_data
!
      call divide_average_ene_sph(icou)
!
!
      call open_tave_ene_spec_data
!
      if(iflag_sph_ene_file .eq. 1) then
        call write_volt_ave_ene_sph_data
      else
        call write_average_ene_sph_data
      end if
!
      call close_ene_spec_data
!
      stop
      end program t_average_sph_ene_spec
