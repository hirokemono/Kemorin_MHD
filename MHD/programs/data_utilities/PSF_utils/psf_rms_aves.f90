!
!     program psf_rms_aves
!
      program psf_rms_aves
!
!      program for pick up surface connectivities form subdomain mesh
!         programmed  by  H.Matsui (U. Chicago)  on Oct. 2003 (ver 1.0)
!         Modified  by  H.Matsui (U. Chicago)  on Jan. 2007 (ver 2.0)
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
      use cal_psf_rms_aves
!
      implicit    none
!
      integer(kind = kint) :: ierr
!
!  ===========
! . for local 
!  ===========
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
      call open_psf_ave_rms_data
      call open_new_psf_ave_rms_data
      call s_cal_psf_rms_aves
!
      stop ' //// program normally terminated //// '
!
!
      end program psf_rms_aves
