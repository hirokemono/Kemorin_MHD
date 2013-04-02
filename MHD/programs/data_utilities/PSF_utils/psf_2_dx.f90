!
!     program psf_2_dx
!
      program psf_2_dx
!
!      program for pick up surface connectivities form subdomain mesh
!         programmed  by  H.Matsui  on May. 2009
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_control_params_4_psf
      use m_control_params_4_iso
      use m_control_data_sections
      use m_control_data_psf_utils
      use set_control_visualizer
      use convert_psf_file
!
      implicit    none
!
!  ===========
! . for local 
!  ===========

      integer(kind=kint), parameter :: iflag_convert = 2
      integer(kind = kint) :: ierr
!
!
      call read_control_data_psf_utils(ierr)
      call set_control_params_4_viz(izero, ierr)
!
      if(ierr .gt. 0) then
        write(*,*) e_message
        stop
      end if
!
      num_psf = num_psf_ctl
      num_iso = num_iso_ctl
!
      call s_convert_psf_file(iflag_convert)
!
      stop
      end program psf_2_dx
