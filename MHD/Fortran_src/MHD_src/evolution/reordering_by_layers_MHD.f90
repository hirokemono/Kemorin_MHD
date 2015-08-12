!reordering_by_layers_MHD.f90
!     module reordering_by_layers_MHD
!
!      Written by H. Matsui on Dec., 2008
!
!     subroutine s_reordering_by_layers_MHD
!
      module reordering_by_layers_MHD
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_reordering_by_layers_MHD
!
      use m_geometry_data
      use m_iccg_parameter
      use m_work_4_MHD_layering
      use m_element_group
      use m_surface_group
!
      use reordering_by_layers
      use reordering_MG_ele_by_layers
!
!
      call allocate_lists_4_layer(ele1%numele)
      call s_reordering_by_layers(ele_grp1, sf_grp1)
!
!   ordereing of element parameters for AMG (for first grid)
!
      if (     ((method_4_solver(1:1).eq.'M')                           &
     &      .or.(method_4_solver(1:1).eq.'m'))                          &
     &   .and. ((method_4_solver(2:2).eq.'G')                           &
     &      .or.(method_4_solver(2:2).eq.'g'))                          &
     &   .and. ((method_4_solver(3:3).eq.'C')                           &
     &      .or.(method_4_solver(3:3).eq.'c'))                          &
     &   .and. ((method_4_solver(4:4).eq.'G')                           &
     &      .or.(method_4_solver(4:4).eq.'g')) ) then
        call reordering_element_first_MG
        call s_reordering_MG_ele_by_layers
      end if
!
      call deallocate_lists_4_layer
!
      end subroutine s_reordering_by_layers_MHD
!
! -----------------------------------------------------------------------
!
      end module reordering_by_layers_MHD
