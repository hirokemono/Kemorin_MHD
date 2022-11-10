!>@file   t_sph_typical_scales.f90
!!@brief      module t_sph_typical_scales
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2022
!
!> @brief  Evaluate dipolarity at CMB
!!
!!@verbatim
!!      subroutine open_typical_scale_file                              &
!!     &         (id_file, ltr, nri, nlayer_ICB, nlayer_CMB,            &
!!     &          kr_in, kr_out, r_in, r_out, tsl)
!!      subroutine write_typical_scale_header                           &
!!     &         (id_file, ltr, nri, nlayer_ICB, nlayer_CMB,            &
!!     &          kr_in, kr_out, r_in, r_out, tsl)
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: ltr, nri
!!        integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
!!        integer(kind = kint), intent(in) :: kr_in, kr_out
!!        real(kind = kreal), intent(in) :: r_in, r_out
!!        type(typical_scale_data), intent(in) :: tsl
!!@endverbatim
!
      module t_sph_typical_scales
!
      use m_precision
      use m_constants
!
      use t_field_labels
!
      implicit none
!
!>        Field label for Elsasser number
!!         @f$ B^{2} / \rho \mu_{0} \eta \Omega @f$
      type(field_def), parameter :: Elsasser_number                     &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Elsasser_number',                         &
     &                math = '$B^{2} / \rho \mu_{0} \eta \Omega $')
!
!>        Field label for dynamic Elsasser number
!!         @f$ B^{2}/ \rho \mu_{0} \Omega u \ell_{B} @f$
      type(field_def), parameter :: dynamic_Elsasser_number             &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'dynamic_Elsasser_number',                 &
     &                math = '$B^{2}/ \rho \mu_{0} \Omega u \ell_{B}$')
!
!>        Field label for dynamic Alfven number
!!         @f$ u \sqrt{\rho \mu_{0}} / B @f$
      type(field_def), parameter :: Alfven_number                       &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Alfven_number',                           &
     &                math = '$u \sqrt{\rho \mu_{0}} / B$')
!
!>        Field label for dynamic Lehnert number
!!         @f$ B / \ell_{B} \Omega \sqrt{\rho \mu_{0}} @f$
      type(field_def), parameter :: Lehnert_number                      &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'Lehnert_number',                          &
     &              math = '$B / \ell_{B} \Omega \sqrt{\rho \mu_{0}}$')
!
!>        Field label for typical flow degree
!!         @f$ \bar{l_{u}} @f$
      type(field_def), parameter :: flow_degree                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'flow_degree',                             &
     &                math = '$ \bar{l_{u}} $')
!
!>        Field label for typical flow order
!!         @f$ \bar{m_{u}} @f$
      type(field_def), parameter :: flow_order                          &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'flow_order',                              &
     &                math = '$ \bar{m_{u}} $')
!
!>        Field label for typical flow degree
!!         @f$ \bar{l_{B}} @f$
      type(field_def), parameter :: magne_degree                        &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'magne_degree',                            &
     &                math = '$ \bar{l_{u}} $')
!
!>        Field label for typical flow order
!!         @f$ \bar{m_{B}} @f$
      type(field_def), parameter :: magne_order                         &
     &    = field_def(n_comp = n_scalar,                                &
     &                name = 'magne_order',                             &
     &                math = '$ \bar{m_{u}} $')
!
!
      type typical_scale_data
!>        Integer flag for typical scales
        integer(kind = kint) :: iflag_ub_scales = 0
!>        File prefix for dipolarity data
        character(len = kchara) :: scale_prefix = 'typical_scales'
!
!>        kinetic energy address
        integer(kind = kint) :: icomp_kene = 0
!>        magnetic energy address
        integer(kind = kint) :: icomp_mene = 0
!
!>        number of output component
        integer(kind = kint) :: num_lscale = 0
!>        magnetic length scale
        real(kind = kreal) :: dl_mag
!>        magnetic zonal length scale
        real(kind = kreal) :: dm_mag
!>        magnetic meridional length scale
        real(kind = kreal) :: dlm_mag
!
!>        kinetic length scale
        real(kind = kreal) :: dl_kin
!>        kinetic zonal length scale
        real(kind = kreal) :: dm_kin
!>        magnetic meridional length scale
        real(kind = kreal) :: dlm_kin
      end type typical_scale_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine open_typical_scale_file                                &
     &         (id_file, ltr, nri, nlayer_ICB, nlayer_CMB,              &
     &          kr_in, kr_out, r_in, r_out, tsl)
!
      use set_parallel_file_name
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ltr, nri
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: r_in, r_out
      type(typical_scale_data), intent(in) :: tsl
!
      character(len = kchara) :: file_name
!
!
      file_name = add_dat_extension(tsl%scale_prefix)
      open(id_file, file = file_name,                                   &
     &    form='formatted', status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_file, file = file_name,                                   &
     &    form='formatted', status='replace')
      call write_typical_scale_header                                   &
     &   (id_file, ltr, nri, nlayer_ICB, nlayer_CMB,                    &
     &    kr_in, kr_out, r_in, r_out, tsl)
!
      end subroutine open_typical_scale_file
!
! -----------------------------------------------------------------------
!
      subroutine write_typical_scale_header                             &
     &         (id_file, ltr, nri, nlayer_ICB, nlayer_CMB,              &
     &          kr_in, kr_out, r_in, r_out, tsl)
!
      use set_parallel_file_name
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: ltr, nri
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: kr_in, kr_out
      real(kind = kreal), intent(in) :: r_in, r_out
      type(typical_scale_data), intent(in) :: tsl
!
      integer(kind = kint) :: i
!
!
      write(id_file,'(a)') 'radial_layers, truncation'
      write(id_file,'(3i16)') nri, ltr
      write(id_file,'(a)')  'ICB_id, CMB_id'
      write(id_file,'(2i16)') nlayer_ICB, nlayer_CMB
      write(id_file,'(a)') 'inner_boundary_ID, inner_boundary_radius'
      write(id_file,'(i16,1pe23.14e3)') kr_in,  r_in
      write(id_file,'(a)') 'outer_boundary_ID, outer_boundary_radius'
      write(id_file,'(i16,1pe23.14e3)') kr_out, r_out
!
      write(id_file,'(a)')   'number_of_fields, number_of_components'
      write(id_file,'(2i16)') tsl%num_lscale, tsl%num_lscale
      write(id_file,'(16i5)') (ione,i=1,tsl%num_lscale)
!
      write(id_file,'(a)',advance='NO')                                 &
     &    't_step    time    '
      if(tsl%icomp_kene .gt. 0) write(id_file,'(a)',advance='NO')       &
     &    'flow_degree  flow_order  '
      if(tsl%icomp_mene .gt. 0) write(id_file,'(a)',advance='NO')       &
     &    'magnetc_degree  magnetic_order'
      write(id_file,'(a)') ''
!
      end subroutine write_typical_scale_header
!
! -----------------------------------------------------------------------
!
      end module t_sph_typical_scales
