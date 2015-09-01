!initialize_phys_data.f90
!------- module initialize_phys_data ---------------------
!
!        programmed by H.Matsui on July, 2006
!
!       subroutine allocate_phys_data
!
      module initialize_phys_data
!
      use m_precision
!
      implicit none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine allocate_phys_data
!
      use m_node_phys_address
      use m_element_phys_address
!
!    integer for work
!
      label_sim = 'GeoFEM_MHD'
!
!  allocation for physical values
!
!      write(*,*) 'allocate_data_arrays'
      call initialize_nod_field_data
!      write(*,*) 'initialize_ele_field_data'
      call initialize_ele_field_data
!
       end subroutine allocate_phys_data
!
!  --------------------------------------------------------------------
!
      end module initialize_phys_data
