!>@file   t_CMB_dipolarity.f90
!!@brief      module t_CMB_dipolarity
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2022
!
!> @brief  Evaluate dipolarity at CMB
!!
!!@verbatim
!!      subroutine alloc_dipolarity_data(num, dip)
!!      subroutine dealloc_dipolarity_data(dip)
!!        integer(kind = kint), intent(in) :: num
!!        type(dipolarity_data), intent(inout) :: dip
!!
!!      subroutine write_dipolarity(i_step, time, ltr, nri,             &
!!     &                            nlayer_ICB, nlayer_CMB, i_magne, dip)
!!        integer, intent(in) :: id_rank
!!        integer(kind = kint), intent(in) :: i_step
!!        real(kind = kreal), intent(in) :: time
!!        integer(kind = kint), intent(in) :: ltr, nri
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        integer(kind = kint), intent(in) :: i_magne
!!        type(dipolarity_data), intent(in) :: dip
!!
!!      subroutine open_dipolarity_file(id_file, ltr, nri,              &
!!     &                                nlayer_ICB, nlayer_CMB, dip)
!!      subroutine write_dipolarity_header(id_file, ltr, nri,           &
!!     &                                   nlayer_ICB, nlayer_CMB, dip)
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: ltr, nri
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        type(dipolarity_data), intent(in) :: dip
!!@endverbatim
!
      module t_CMB_dipolarity
!
      use m_precision
      use m_constants
!
      use t_field_labels
      use t_read_sph_spectra
!
      implicit none
!
!>        Field label for dipolarity
!!         @f$ f_{dip} @f$
      type(field_def), parameter :: dipolarity                          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'dipolarity',                              &
     &                math = '$ f_{dip} $')
!
!
      type dipolarity_data
!>        Integer flag for dipolarity
        integer(kind = kint) :: iflag_dipolarity = 0
!>        File prefix for dipolarity data
        character(len = kchara)                                         &
     &                 :: dipolarity_file_name = 'dipolarity.dat'
!
!>        Radial address for dipolarity
        integer(kind = kint) :: krms_CMB
!>        Radius for dipolarity
        real(kind = kreal) :: rdip_CMB
!
!>        magnetic energy address
        integer(kind = kint) :: icomp_mene = 0
!
!>        Truncation degree to evaluate dipolarity
        integer(kind = kint) :: num_dip
!>        Name of each dipolarity data
        character(len = kchara), allocatable :: dip_name(:)
!>        Truncation degree to evaluate dipolarity
        integer(kind = kint), allocatable :: ltr_max(:)
!>        Dipolarity
        real(kind = kreal), allocatable :: f_dip(:)
      end type dipolarity_data
!
      integer(kind = kint), parameter, private :: id_dipolarity = 36
      character(len = kchara), parameter                                &
     &                        :: dip_ltr_label = 'truncation_'
!
      type(sph_spectr_head_labels), parameter, private                  &
     &            :: sph_dipolarity_labels = sph_spectr_head_labels(    &
     &                           hdr_nri = 'radial_layers',             &
     &                           hdr_ltr = 'truncation',                &
     &                           hdr_ICB_id = 'ICB_id',                 &
     &                           hdr_CMB_id = 'CMB_id',                 &
     &                           hdr_kr_in =  'Not_used',               &
     &                           hdr_r_in =   'Not_used',               &
     &                           hdr_kr_out = 'Upper_boundary_ID',      &
     &                           hdr_r_out =  'Upper_boundary_radius',  &
     &                           hdr_num_field = 'Number_of_field',     &
     &                           hdr_num_comp = 'Number_of_components')
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_dipolarity_data(num, dip)
!
      integer(kind = kint), intent(in) :: num
      type(dipolarity_data), intent(inout) :: dip
!
!
      dip%num_dip = num
      allocate(dip%dip_name(dip%num_dip))
      allocate(dip%ltr_max(dip%num_dip))
      allocate(dip%f_dip(dip%num_dip))
!
      dip%ltr_max(1:dip%num_dip) = -1
      dip%f_dip(1:dip%num_dip) =    0.0d0
!
      end subroutine alloc_dipolarity_data
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_dipolarity_data(dip)
!
      type(dipolarity_data), intent(inout) :: dip
!
      deallocate(dip%f_dip, dip%ltr_max, dip%dip_name)
!
      end subroutine dealloc_dipolarity_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_dipolarity(i_step, time, ltr, nri,               &
     &                            nlayer_ICB, nlayer_CMB, i_magne, dip)
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      integer(kind = kint), intent(in) :: ltr, nri
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: i_magne
      type(dipolarity_data), intent(in) :: dip
!
      integer(kind = kint) :: i
!
!
      if(dip%iflag_dipolarity .le. izero) return
      if(i_magne .le. 0) return
!
      call open_dipolarity_file(id_dipolarity, ltr, nri,                &
     &                          nlayer_ICB, nlayer_CMB, dip)
!
      write(id_dipolarity,'(i16,1pe23.14e3)',advance='NO') i_step, time
      do i = 1, dip%num_dip
        write(id_dipolarity,'(1pe23.14e3)',advance='NO') dip%f_dip(i)
      end do
      write(id_dipolarity,'(a)') ''
      close(id_dipolarity)
!
      end subroutine write_dipolarity
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_dipolarity_file(id_file, ltr, nri,                &
     &                                nlayer_ICB, nlayer_CMB, dip)
!
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ltr, nri
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(dipolarity_data), intent(in) :: dip
!
!
      open(id_file, file = dip%dipolarity_file_name, form='formatted',  &
     &     status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_file, file = dip%dipolarity_file_name,                    &
     &     form='formatted', status='replace')
!
      call write_dipolarity_header(id_file, ltr, nri,                   &
     &                             nlayer_ICB, nlayer_CMB, dip)
!
      end subroutine open_dipolarity_file
!
! -----------------------------------------------------------------------
!
      subroutine write_dipolarity_header(id_file, ltr, nri,             &
     &                                   nlayer_ICB, nlayer_CMB, dip)
!
      use sph_power_spectr_data_text
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ltr, nri
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(dipolarity_data), intent(in) :: dip
!
      type(read_sph_spectr_data) :: sph_OUT
      integer(kind = kint) :: len_tot
      integer(kind = kint) :: len_each(6)
!
!
      call dup_dipolarity_header_to_IO                                  &
     &   (ltr, nri, nlayer_ICB, nlayer_CMB, dip, sph_OUT)
!
      call len_sph_vol_spectr_header(sph_dipolarity_labels, sph_OUT,    &
     &                               len_each, len_tot)
      write(id_file,'(a)',ADVANCE='NO')                                 &
     &       sph_vol_spectr_header_text(len_tot, len_each,              &
     &                                  sph_dipolarity_labels, sph_OUT)
      call dealloc_sph_espec_data(sph_OUT)
!
      end subroutine write_dipolarity_header
!
! -----------------------------------------------------------------------
!
      subroutine dup_dipolarity_header_to_IO                            &
     &         (ltr, nri, nlayer_ICB, nlayer_CMB, dip, sph_OUT)
!
      use m_time_labels
!
      integer(kind = kint), intent(in) :: ltr, nri
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(dipolarity_data), intent(in) :: dip
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
      integer(kind = kint) :: icou
!
!
      sph_OUT%ltr_sph = ltr
      sph_OUT%nri_sph = nri
      sph_OUT%nri_dat = 1
      sph_OUT%kr_ICB =  nlayer_ICB
      sph_OUT%kr_CMB =  nlayer_CMB
      sph_OUT%kr_inner = izero
      sph_OUT%kr_outer = dip%krms_CMB
      sph_OUT%r_inner =  zero
      sph_OUT%r_outer =  dip%rdip_CMB
!
      sph_OUT%nfield_sph_spec = dip%num_dip
      sph_OUT%ntot_sph_spec =   dip%num_dip
      sph_OUT%num_time_labels = 2
      call alloc_sph_espec_name(sph_OUT)
      call alloc_sph_spectr_data(izero, sph_OUT)
!
      sph_OUT%ene_sph_spec_name(1) = fhd_t_step
      sph_OUT%ene_sph_spec_name(2) = fhd_time
      icou = sph_OUT%num_time_labels
      sph_OUT%ene_sph_spec_name(icou+1:icou+dip%num_dip)                &
     &                           = dip%dip_name(1:dip%num_dip)
      sph_OUT%ncomp_sph_spec(1:dip%num_dip) = 1
!
      end subroutine dup_dipolarity_header_to_IO
!
! -----------------------------------------------------------------------
!
      end module t_CMB_dipolarity
