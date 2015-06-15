!m_filter_dxdxi.f90
!      module m_filter_dxdxi
!
!     Written by H. Matsui
!
!       subroutine allocate_jacobians_on_node
!       subroutine allocate_jacobians_for_ele
!       subroutine deallocate_jacobians_on_node
!!
!!          1st difference of elengh_nod
!!              (node ID, direction of diffrence)
!!      dxdxi_nod(:)... filter_dxi1%dxi_nod%dx%df_dxi(:)
!!      dxdei_nod(:)... filter_dxi1%dxi_nod%dx%df_dei(:)
!!      dxdzi_nod(:)... filter_dxi1%dxi_nod%dx%df_dzi(:)
!!      dydxi_nod(:)... filter_dxi1%dxi_nod%dy%df_dxi(:)
!!      dydei_nod(:)... filter_dxi1%dxi_nod%dy%df_dei(:)
!!      dydzi_nod(:)... filter_dxi1%dxi_nod%dy%df_dzi(:)
!!      dzdxi_nod(:)... filter_dxi1%dxi_nod%dz%df_dxi(:)
!!      dzdei_nod(:)... filter_dxi1%dxi_nod%dz%df_dei(:)
!!      dzdzi_nod(:)... filter_dxi1%dxi_nod%dz%df_dzi(:)
!!
!!          1st difference of elengh_nod
!!              (node ID, direction of diffrence)
!!      dxdxi_ele(:)... filter_dxi1%dxi_ele%dx%df_dxi(:)
!!      dxdei_ele(:)... filter_dxi1%dxi_ele%dx%df_dei(:)
!!      dxdzi_ele(:)... filter_dxi1%dxi_ele%dx%df_dzi(:)
!!      dydxi_ele(:)... filter_dxi1%dxi_ele%dy%df_dxi(:)
!!      dydei_ele(:)... filter_dxi1%dxi_ele%dy%df_dei(:)
!!      dydzi_ele(:)... filter_dxi1%dxi_ele%dy%df_dzi(:)
!!      dzdxi_ele(:)... filter_dxi1%dxi_ele%dz%df_dxi(:)
!!      dzdei_ele(:)... filter_dxi1%dxi_ele%dz%df_dei(:)
!!      dzdzi_ele(:)... filter_dxi1%dxi_ele%dz%df_dzi(:)
!!@endverbatim
!
      module m_filter_dxdxi
!
      use m_precision
      use t_filter_dxdxi
!
      implicit none
!
!>        Structure for dx/dxi at nodes and elements
!!          1st difference of elen_nod
!!              (node ID, direction of diffrence)
      type(gradient_model_data_type), save :: filter_dxi1
!
      end module m_filter_dxdxi
