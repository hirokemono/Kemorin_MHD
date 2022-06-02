!>@file   t_no_heat_Nusselt.f90
!!@brief  module t_no_heat_Nusselt
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays for Nusselt number
!!
!!@verbatim
!!      subroutine set_ctl_params_no_heat_Nu                            &
!!     &         (Nusselt_file_prefix, rj_fld, Nu_type)
!!        type(read_character_item), intent(in) :: Nusselt_file_prefix
!!        type(phys_data), intent(in) :: rj_fld
!!        type(nusselt_number_data), intent(inout) :: Nu_type
!!
!!      subroutine write_no_heat_source_Nu(idx_rj_degree_zero,          &
!!     &          i_step, time, Nu_type)
!!
!!      subroutine open_read_no_heat_source_Nu(id_pick, Nu_type)
!!      subroutine read_no_heat_source_Nu                               &
!!     &         (id_pick, i_step, time, Nu_type, ierr)
!!@endverbatim
!
      module t_no_heat_Nusselt
!
      use m_precision
      use m_constants
      use t_sph_matrix
!
      implicit  none
!
!
!>      File ID for Nusselt number IO
      integer(kind = kint), parameter :: id_Nusselt = 23
!
!>      Structure for Nusselt number data
      type nusselt_number_data
!>        Output flag for Nusselt number IO
        integer(kind = kint) :: iflag_no_source_Nu = 0
!>        File prefix for Nusselt number file
        character(len = kchara) :: Nusselt_file_head = 'Nusselt'
!
!>        Radius at inner boundary
        real(kind = kreal) :: r_ICB_Nu
!>        Radius at outer boundary
        real(kind = kreal) :: r_CMB_Nu
!>        Nusselt number at inner boundary
        real(kind = kreal) :: Nu_ICB
!>        Nusselt number at outer boundary
        real(kind = kreal) :: Nu_CMB
!
!>        Matrix to solve diffusive profile
        type(band_matrix_type) :: band_s00_poisson_fixS
      end type nusselt_number_data
!
      private :: id_Nusselt
      private :: open_no_heat_source_Nu
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_ctl_params_no_heat_Nu                              &
     &         (Nusselt_file_prefix, rj_fld, Nu_type)
!
      use m_base_field_labels
      use m_grad_field_labels
      use t_phys_data
      use t_control_array_character
!
      type(read_character_item), intent(in) :: Nusselt_file_prefix
      type(phys_data), intent(in) :: rj_fld
      type(nusselt_number_data), intent(inout) :: Nu_type
!
      integer(kind = kint) :: i
!
!    Turn On Nusselt number if temperature gradient is there
      Nu_type%iflag_no_source_Nu = 0
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. grad_temp%name) then
          Nu_type%iflag_no_source_Nu = 1
          exit
        end if
      end do
!
      if(Nusselt_file_prefix%iflag .gt. 0) then
        Nu_type%iflag_no_source_Nu = 1
        Nu_type%Nusselt_file_head = Nusselt_file_prefix%charavalue
      else
        Nu_type%iflag_no_source_Nu = 0
      end if
!
!    Turn Off Nusselt number if heat source is there
      do i = 1, rj_fld%num_phys
        if(rj_fld%phys_name(i) .eq. heat_source%name) then
          Nu_type%iflag_no_source_Nu = 0
          exit
        end if
      end do
!
      end subroutine set_ctl_params_no_heat_Nu
!
! -----------------------------------------------------------------------
!
      subroutine open_no_heat_source_Nu(Nu_type)
!
      use set_parallel_file_name
      use write_field_labels
!
      type(nusselt_number_data), intent(in) :: Nu_type
      character(len = kchara) :: file_name
!
!
      file_name = add_dat_extension(Nu_type%Nusselt_file_head)
      open(id_Nusselt, file = file_name,                                &
     &    form='formatted', status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_Nusselt, file = file_name,                                &
     &    form='formatted', status='replace')
!
!
      write(id_Nusselt,'(a)')    '# Inner_radius, Outer_radius'
      write(id_Nusselt,'(1p2e25.15e3)')                                 &
     &                         Nu_type%r_ICB_Nu, Nu_type%r_CMB_Nu
!
      write(id_Nusselt,'(a)',advance='NO')                              &
     &    't_step    time    Nu_ICB    Nu_CMB'
      write(id_Nusselt,'(a)') ''
!
      end subroutine open_no_heat_source_Nu
!
! -----------------------------------------------------------------------
!
      subroutine write_no_heat_source_Nu(idx_rj_degree_zero,            &
     &          i_step, time, Nu_type)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero, i_step
      real(kind = kreal), intent(in) :: time
      type(nusselt_number_data), intent(in) :: Nu_type
!
!
      if(Nu_type%iflag_no_source_Nu .eq. izero) return
      if(idx_rj_degree_zero .eq. izero) return
!
      call open_no_heat_source_Nu(Nu_type)
!
      write(id_Nusselt,'(i16,1p3e23.14e3)')                             &
     &       i_step, time, Nu_type%Nu_ICB, Nu_type%Nu_CMB
!
      close(id_Nusselt)
!
      end subroutine write_no_heat_source_Nu
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine open_read_no_heat_source_Nu(id_pick, Nu_type)
!
      use skip_comment_f
      use set_parallel_file_name
!
      integer(kind = kint), intent(in) :: id_pick
      type(nusselt_number_data), intent(inout) :: Nu_type
!
      character(len = kchara) :: file_name
!
      integer(kind = kint) :: i
      character(len=255) :: tmpchara
!
!
      file_name = add_dat_extension(Nu_type%Nusselt_file_head)
      open(id_pick, file = file_name)
!
      call skip_comment(tmpchara,id_pick)
      read(tmpchara,*) Nu_type%r_ICB_Nu, Nu_type%r_CMB_Nu
!
      read(id_pick,*) (tmpchara,i=1,4)
!
      end subroutine open_read_no_heat_source_Nu
!
! -----------------------------------------------------------------------
!
      subroutine read_no_heat_source_Nu                                 &
     &         (id_pick, i_step, time, Nu_type, ierr)
!
      integer(kind = kint), intent(in) :: id_pick
      integer(kind = kint), intent(inout) :: i_step, ierr
      real(kind = kreal), intent(inout) :: time
      type(nusselt_number_data), intent(inout) :: Nu_type
!
!
      ierr = 0
      read(id_pick,*,err=99,end=99)                                     &
     &        i_step, time, Nu_type%Nu_ICB, Nu_type%Nu_CMB
!
      return
!
   99 continue
      ierr = 1
      return
!
      end subroutine read_no_heat_source_Nu
!
! -----------------------------------------------------------------------
!
      end module t_no_heat_Nusselt
