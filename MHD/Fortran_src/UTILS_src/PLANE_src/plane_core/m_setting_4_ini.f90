!
!      module m_setting_4_ini
!
!      subroutine set_initial_components
!      subroutine add_initial_num_comp_mhd
!      subroutine add_initial_comp_mhd
!
      module m_setting_4_ini
!
      use m_precision
!
      use m_phys_labels
!
      implicit none
!
!
      integer(kind=kint) :: num_rst_org, num_rst_new, num_added_merge
      integer(kind=kint) :: ntot_rst_org, ntot_rst_new
!
      integer(kind=kint), dimension(6), private :: n_phys_id
!
      private :: num_added_merge
!
!-----------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------
!
      subroutine set_initial_components
!
      use m_geometry_data_4_merge
!
      integer(kind=kint) :: np, na
!
      write(*,*) 'Physical values for initial value'
      write(*,*) ' 1: ', trim(fhd_velo)
      write(*,*) ' 2: ', trim(fhd_temp)
      write(*,*) ' 3: ', trim(fhd_press)
      write(*,*) ' 4: ', trim(fhd_vecp)
      write(*,*) ' 5: ', trim(fhd_magne)
      write(*,*) ' 6: ', trim(fhd_mag_potential)
      write(*,*) ''
!
      write(*,*) 'number of physical values'
      read(*,*) merged_fld%num_phys
!
      call allocate_merged_field_name
!
      write(*,*) ' select physical values'
!       write(*,*) ' No. ', np
      read(*,*) (n_phys_id(np),np=1, merged_fld%num_phys)
!
!
      merged_fld%istack_component(0) = 0
      do np = 1, merged_fld%num_phys
        if ( n_phys_id(np) .eq. 1 ) then
          merged_fld%phys_name(np) = fhd_velo
          merged_fld%num_component(np) = 3
        else if ( n_phys_id(np) .eq. 2 ) then
          merged_fld%phys_name(np) = fhd_temp
          merged_fld%num_component(np) = 1
        else if ( n_phys_id(np) .eq. 3 ) then
          merged_fld%phys_name(np) = fhd_press
          merged_fld%num_component(np) = 1
        else if ( n_phys_id(np) .eq. 4 ) then
          merged_fld%phys_name(np) = fhd_vecp
          merged_fld%num_component(np) = 3
        else if ( n_phys_id(np) .eq. 5 ) then
          merged_fld%phys_name(np) = fhd_magne
          merged_fld%num_component(np) = 3
        else if ( n_phys_id(np) .eq. 6 ) then
          merged_fld%phys_name(np) = fhd_mag_potential
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
       subroutine add_initial_num_comp_mhd
!
      use m_geometry_data_4_merge
!
!
      write(*,*) 'input number of physical valuesto add'
      read(*,*) num_added_merge
      num_rst_org = merged_fld%num_phys
      num_rst_new = num_rst_org + num_added_merge
!
      end subroutine add_initial_num_comp_mhd
!
!-----------------------------------------------------------------
!
       subroutine add_initial_comp_mhd
!
      use m_geometry_data_4_merge
!
      integer(kind=kint) :: np, nq
!
!
      write(*,*) 'aviable physical values for initial value'
      write(*,*) ' 4: ', trim(fhd_vecp)
      write(*,*) ' 5: ', trim(fhd_magne)
      write(*,*) ' 6: ', trim(fhd_mag_potential)
!
      write(*,*) ' select physical values'
      do np = 1, num_added_merge
       write(*,*) ' No. ', np
       read(*,*) n_phys_id(np)
      end do
!
      do np = 1, num_added_merge
        nq = num_rst_org + np
        if ( n_phys_id(np) .eq. 4 ) then
          merged_fld%phys_name(nq) = fhd_vecp
          merged_fld%num_component(nq) = 3
        else if ( n_phys_id(np) .eq. 5 ) then
          merged_fld%phys_name(nq) = fhd_magne
          merged_fld%num_component(nq) = 3
        else if ( n_phys_id(np) .eq. 6 ) then
          merged_fld%phys_name(nq) = fhd_mag_potential
          merged_fld%num_component(nq) = 1
        end if
        merged_fld%istack_component(nq)                                 &
     &         = merged_fld%istack_component(nq-1)                      &
     &          + merged_fld%num_component(nq)
      end do
      ntot_rst_new = merged_fld%istack_component(num_rst_new)
!
      end subroutine add_initial_comp_mhd
!
!-----------------------------------------------------------------
!
      end module m_setting_4_ini
