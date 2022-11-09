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
!!      subroutine set_ctl_dipolarity_params                            &
!!     &         (fdip_file_prefix, fdip_truncation, rj_fld, dip)
!!        type(read_character_item), intent(in) :: fdip_file_prefix
!!        type(ctl_array_int), intent(in) :: fdip_truncation
!!        type(phys_data), intent(in) :: rj_fld
!!        type(dipolarity_data), intent(inout) :: dip
!!      subroutine write_dipolarity(i_step, time, ltr, nri,             &
!!     &                            nlayer_ICB, nlayer_CMB, i_magne, dip)
!!        integer, intent(in) :: id_rank
!!        integer(kind = kint), intent(in) :: i_step
!!        real(kind = kreal), intent(in) :: time
!!        integer(kind = kint), intent(in) :: ltr, nri
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        integer(kind = kint), intent(in) :: i_magne
!!        type(dipolarity_data), intent(in) :: dip
!!      subroutine cal_CMB_dipolarity(id_rank, rj_fld, pwr, dip)
!!        integer, intent(in) :: id_rank
!!        type(phys_data), intent(in) :: rj_fld
!!        type(sph_mean_squares), intent(in) :: pwr
!!        real(kind = kreal), intent(inout) :: f_dip
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
      use t_phys_data
      use t_rms_4_sph_spectr
      use t_spheric_parameter
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
      subroutine set_ctl_dipolarity_params                              &
     &         (fdip_file_prefix, fdip_truncation, rj_fld, dip)
!
      use m_base_field_labels
      use t_phys_data
      use t_control_array_character
      use t_control_array_integer
      use set_parallel_file_name
!
      type(read_character_item), intent(in) :: fdip_file_prefix
      type(ctl_array_int), intent(in) :: fdip_truncation
      type(phys_data), intent(in) :: rj_fld
      type(dipolarity_data), intent(inout) :: dip
!
      integer(kind = kint) :: i, num
!
!    Turn On Nusselt number if temperature gradient is there
      dip%iflag_dipolarity = 0
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. magnetic_field%name) then
          dip%iflag_dipolarity = 1
          exit
        end if
      end do
!
      if(fdip_file_prefix%iflag .gt. 0) then
        dip%iflag_dipolarity = 1
        dip%dipolarity_file_name                                        &
     &              = add_dat_extension(fdip_file_prefix%charavalue)
      else
        dip%iflag_dipolarity = 0
      end if
!
      if(dip%iflag_dipolarity .gt. 0) then
        num = 1
        if(fdip_truncation%num .gt. 0) then
          num = fdip_truncation%num + 1
        end if
        call alloc_dipolarity_data(num, dip)
!
        dip%ltr_max(1) = -1
        do i = 2, dip%num_dip
          dip%ltr_max(i) = fdip_truncation%ivec(i-1)
        end do
      end if
!
      end subroutine set_ctl_dipolarity_params
!
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
!
      subroutine cal_CMB_dipolarity(id_rank, rj_fld, pwr, dip)
!
      use t_spheric_parameter
      use t_spheric_rj_data
!
      integer, intent(in) :: id_rank
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      type(dipolarity_data), intent(inout) :: dip
!
!>      magnetic energy at CMB
      real(kind = kreal) :: me_cmb_d
!>      dipole component of magnetic energy at CMB
      real(kind = kreal) :: pwr_g10
!
      integer(kind = kint) :: i, l
!
!
      if(dip%iflag_dipolarity .le. izero) return
!
      if(dip%icomp_mene .le. 0)                                         &
     &          dip%icomp_mene = find_rms_address_4_mene(pwr, rj_fld)
      if(dip%icomp_mene .le. 0) return
!
      if(id_rank .eq. pwr%irank_l) then
        pwr_g10 = pwr%shl_l(dip%krms_CMB,1,dip%icomp_mene)
!
        do i = 1, dip%num_dip
          me_cmb_d = 0.0d0
!$omp parallel do reduction(+:me_cmb_d)
          do l = 1, dip%ltr_max(i)
            me_cmb_d = me_cmb_d                                         &
     &                + pwr%shl_l(dip%krms_CMB,l,dip%icomp_mene)
          end do
!$omp end parallel do
          dip%f_dip(i) = pwr_g10 / me_cmb_d
        end do
      end if
!
      end subroutine cal_CMB_dipolarity
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
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ltr, nri
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      type(dipolarity_data), intent(in) :: dip
!
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)') '# radial_layers, truncation'
      write(id_file,'(3i16)') nri, ltr
      write(id_file,'(a)')  '# ICB_id, CMB_id'
      write(id_file,'(2i16)') nlayer_ICB, nlayer_CMB
      write(id_file,'(a)') '# Not used'
      write(id_file,'(i16,1pe23.14e3)')                                 &
     &                     izero, zero
      write(id_file,'(a)') '# Radis address and radius for dipolarity'
      write(id_file,'(i16,1pe23.14e3)')                                 &
     &                     dip%krms_CMB, dip%rdip_CMB
!
      write(id_file,'(a)')    'number of components'
      write(id_file,'(2i16)') dip%num_dip, dip%num_dip
      write(id_file,'(16i5)') (ione,i=1,dip%num_dip)
!
      write(id_file,'(a)',advance='NO') 't_step    time    '
      do i = 1, dip%num_dip
        write(id_file,'(a, a4)',advance='NO')                           &
     &                                 trim(dip%dip_name(i)), '    '
      end do
      write(id_file,'(a)') ''
!
      end subroutine write_dipolarity_header
!
! -----------------------------------------------------------------------
!
      integer(kind = kint)                                              &
     &          function find_rms_address_4_mene(pwr, rj_fld)
!
      use m_base_field_labels
      use t_rms_4_sph_spectr
!
      type(phys_data), intent(in) :: rj_fld
      type(sph_mean_squares), intent(in) :: pwr
!
      integer(kind = kint) :: j_fld, i_fld
!
      do j_fld = 1, pwr%num_fld_sq
        i_fld = pwr%id_field(j_fld)
!
        find_rms_address_4_mene = 0
        if(rj_fld%phys_name(i_fld) .eq. magnetic_field%name) then
          find_rms_address_4_mene = pwr%istack_comp_sq(j_fld-1) + 1
          exit
        end if
      end do
!
      end function find_rms_address_4_mene
!
! -----------------------------------------------------------------------
!
      end module t_CMB_dipolarity
