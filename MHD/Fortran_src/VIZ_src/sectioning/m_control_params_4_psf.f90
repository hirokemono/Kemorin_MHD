!
!      module m_control_params_4_psf
!
!        programmed by H.Matsui on May. 2006
!
!      subroutine allocate_control_params_4_psf
!
      module m_control_params_4_psf
!
      use m_precision
!
      implicit  none
!
!
      integer(kind = kint), parameter :: psf_ctl_file_code = 11
!
      integer(kind = kint) :: num_psf
      character(len = kchara), target, allocatable :: psf_header(:)
      integer(kind = kint), target, allocatable :: itype_psf_file(:)
!
      integer(kind = kint), allocatable :: id_section_method(:)
!
!
      real(kind = kreal), allocatable :: const_psf(:,:)
!   coefficients for surface equation
!
      integer(kind = kint), allocatable :: id_psf_group(:)
!
      integer(kind = kint) :: max_ncomp_psf_out
!  number and stack of component for each surfaces
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_control_params_4_psf
!
      use m_field_file_format
!
!
      allocate(psf_header(num_psf))
      allocate(itype_psf_file(num_psf))
!
      allocate(id_section_method(num_psf))
!
      allocate(const_psf(10,num_psf))
!
      allocate(id_psf_group(num_psf))
!
      itype_psf_file =   iflag_udt
      id_section_method =  0
      id_psf_group =       0
!
      const_psf = 0.0d0
!
      end subroutine allocate_control_params_4_psf
!
!  ---------------------------------------------------------------------
!
      end module m_control_params_4_psf
