!
!      module set_coefs_of_sections
!
!        programmed by H.Matsui on May. 2006
!
!!    subroutine s_set_coefs_of_sections                                &
!!     &         (psf, id_section_method, const_psf, ierr)
!
      module set_coefs_of_sections
!
      use m_precision
!
      implicit  none
!
      character(len = kchara), parameter :: cflag_eq =  'equation'
      character(len = kchara), parameter :: cflag_pln = 'plane'
      character(len = kchara), parameter :: cflag_sph = 'sphere'
      character(len = kchara), parameter :: cflag_elp = 'ellipsoid'
      character(len = kchara), parameter :: cflag_hyp = 'hyperboloid'
      character(len = kchara), parameter :: cflag_prb = 'paraboloid'
      character(len = kchara), parameter :: cflag_grp = 'group'
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine s_set_coefs_of_sections                                &
     &         (psf, id_section_method, const_psf, ierr)
!
      use m_error_IDs
      use m_control_data_4_psf
      use t_psf_patch_data
      use set_cross_section_coefs
!
      type(psf_ctl), intent(inout) :: psf
!
      integer(kind = kint), intent(inout)  :: id_section_method
      real(kind = kreal), intent(inout) :: const_psf(10)
      integer(kind = kint), intent(inout) :: ierr
!
      character(len = kchara) :: tmpchara
!
!
      ierr = 0
      tmpchara = psf%section_method_ctl%charavalue
!
      if(cmp_no_case(tmpchara, cflag_eq)) then
        id_section_method = 1
        call set_coefs_4_psf(psf%psf_coefs_ctl%num,                     &
     &      psf%psf_coefs_ctl%c_tbl,  psf%psf_coefs_ctl%vect,           &
     &      const_psf(1) )
        call deallocate_psf_coefs_ctl(psf)
!
      else if(cmp_no_case(tmpchara, cflag_pln)) then
        id_section_method = 2
        call set_coefs_4_plane(psf, const_psf(1))
        call deallocate_psf_center_ctl(psf)
        call deallocate_psf_normal_ctl(psf)
!
      else if(cmp_no_case(tmpchara, cflag_sph)) then
        id_section_method = 2
        call set_coefs_4_sphere(psf, const_psf(1))
        call deallocate_psf_center_ctl(psf)
!
      else if(cmp_no_case(tmpchara, cflag_elp)) then
        id_section_method = 3
        call set_coefs_4_ellipsode(psf, const_psf(1) )
        call deallocate_psf_axis_ctl(psf)
        call deallocate_psf_center_ctl(psf)
!
      else if(cmp_no_case(tmpchara, cflag_hyp)) then
        id_section_method = 4
        call set_coefs_4_hyperboloide(psf, const_psf(1) )
        call deallocate_psf_axis_ctl(psf)
        call deallocate_psf_center_ctl(psf)
!
      else if(cmp_no_case(tmpchara, cflag_prb)) then
        id_section_method = 5
        call set_coefs_4_parabolic(psf, const_psf(1) )
        call deallocate_psf_axis_ctl(psf)
        call deallocate_psf_center_ctl(psf)
      else
        ierr = ierr_VIZ
        write(e_message,'(a)') 'Set cross section mode'
        return
      end if
!
      end subroutine s_set_coefs_of_sections
!
!  ---------------------------------------------------------------------
!
      end module set_coefs_of_sections
