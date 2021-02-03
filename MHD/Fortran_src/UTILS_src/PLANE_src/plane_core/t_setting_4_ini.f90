!
!      module t_setting_4_ini
!
!!      subroutine set_initial_components(merged_fld)
!!        type(phys_data), intent(inout) :: merged_fld
!!      subroutine add_initial_num_comp_mhd(merged_fld, pini_p)
!!        type(phys_data), intent(in) :: merged_fld
!!        type(plane_initial_setting), intent(inout) :: pini_p
!!      subroutine add_initial_comp_mhd(merged_fld, pini_p)
!!        type(phys_data), intent(inout) :: merged_fld
!!        type(plane_initial_setting), intent(inout) :: pini_p
!
      module t_setting_4_ini
!
      use m_precision
!
      use t_phys_data
!
      implicit none
!
!
      type plane_initial_setting
        integer(kind=kint) :: num_rst_org
        integer(kind=kint) :: num_rst_new

        integer(kind=kint) :: ntot_rst_org
        integer(kind=kint) :: ntot_rst_new
!
        integer(kind=kint), private :: n_phys_id(6)
      end type plane_initial_setting
!
!-----------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------
!
      subroutine set_initial_components(merged_fld)
!
      use m_base_field_labels
!
      type(phys_data), intent(inout) :: merged_fld
!
      type(plane_initial_setting) :: pini_p
      integer(kind=kint) :: np
!
      write(*,*) 'Physical values for initial value'
      write(*,*) ' 1: ', trim(velocity%name)
      write(*,*) ' 2: ', trim(temperature%name)
      write(*,*) ' 3: ', trim(pressure%name)
      write(*,*) ' 4: ', trim(vector_potential%name)
      write(*,*) ' 5: ', trim(magnetic_field%name)
      write(*,*) ' 6: ', trim(magnetic_potential%name)
      write(*,*) ''
!
      write(*,*) 'number of physical values'
      read(*,*) merged_fld%num_phys
!
      call alloc_phys_name(merged_fld)
!
      write(*,*) ' select physical values'
!       write(*,*) ' No. ', np
      read(*,*) pini_p%n_phys_id(1:merged_fld%num_phys)
!
!
      merged_fld%istack_component(0) = 0
      do np = 1, merged_fld%num_phys
        if (pini_p%n_phys_id(np) .eq. 1 ) then
          merged_fld%phys_name(np) = velocity%name
          merged_fld%num_component(np) = 3
        else if(pini_p%n_phys_id(np) .eq. 2 ) then
          merged_fld%phys_name(np) = temperature%name
          merged_fld%num_component(np) = 1
        else if(pini_p%n_phys_id(np) .eq. 3 ) then
          merged_fld%phys_name(np) = pressure%name
          merged_fld%num_component(np) = 1
        else if(pini_p%n_phys_id(np) .eq. 4 ) then
          merged_fld%phys_name(np) = vector_potential%name
          merged_fld%num_component(np) = 3
        else if(pini_p%n_phys_id(np) .eq. 5 ) then
          merged_fld%phys_name(np) = magnetic_field%name
          merged_fld%num_component(np) = 3
        else if(pini_p%n_phys_id(np) .eq. 6 ) then
          merged_fld%phys_name(np) = magnetic_potential%name
          merged_fld%num_component(np) = 1
        end if
!
        merged_fld%istack_component(np)                                 &
     &       = merged_fld%istack_component(np-1)                        &
     &        + merged_fld%num_component(np)
      end do
      merged_fld%ntot_phys = merged_fld%istack_component(np)
!
       end subroutine set_initial_components
!
!-----------------------------------------------------------------
!
       subroutine add_initial_num_comp_mhd(merged_fld, pini_p)
!
      type(phys_data), intent(in) :: merged_fld
      type(plane_initial_setting), intent(inout) :: pini_p
!
      integer(kind=kint) :: num_added_merge
!
!
      write(*,*) 'input number of physical valuesto add'
      read(*,*) num_added_merge
      pini_p%num_rst_org = merged_fld%num_phys
      pini_p%num_rst_new = pini_p%num_rst_org + num_added_merge
!
      end subroutine add_initial_num_comp_mhd
!
!-----------------------------------------------------------------
!
      subroutine add_initial_comp_mhd(merged_fld, pini_p)
!
      use m_base_field_labels
!
      type(phys_data), intent(inout) :: merged_fld
      type(plane_initial_setting), intent(inout) :: pini_p
!
      integer(kind=kint) :: num_added_merge
      integer(kind=kint) :: np, nq
!
!
      write(*,*) 'aviable physical values for initial value'
      write(*,*) ' 4: ', trim(vector_potential%name)
      write(*,*) ' 5: ', trim(magnetic_field%name)
      write(*,*) ' 6: ', trim(magnetic_potential%name)
!
      write(*,*) ' select physical values'
      do np = 1, num_added_merge
       write(*,*) ' No. ', np
       read(*,*) pini_p%n_phys_id(np)
      end do
!
      do np = 1, num_added_merge
        nq = pini_p%num_rst_org + np
        if(pini_p%n_phys_id(np) .eq. 4 ) then
          merged_fld%phys_name(nq) = vector_potential%name
          merged_fld%num_component(nq) = 3
        else if(pini_p%n_phys_id(np) .eq. 5 ) then
          merged_fld%phys_name(nq) = magnetic_field%name
          merged_fld%num_component(nq) = 3
        else if(pini_p%n_phys_id(np) .eq. 6 ) then
          merged_fld%phys_name(nq) = magnetic_potential%name
          merged_fld%num_component(nq) = 1
        end if
        merged_fld%istack_component(nq)                                 &
     &         = merged_fld%istack_component(nq-1)                      &
     &          + merged_fld%num_component(nq)
      end do
      pini_p%ntot_rst_new                                               &
     &      = merged_fld%istack_component(pini_p%num_rst_new)
!
      end subroutine add_initial_comp_mhd
!
!-----------------------------------------------------------------
!
      end module t_setting_4_ini
