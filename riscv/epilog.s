#
# Copyright (c) 2020 Michael Roe
#
# Unless required by applicable law or agreed to in writing, Work distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations under the License.

.global _fail
_fail:
        slli t0, a1, 1
        ori t0, t0, 1
        la t1, tohost
	sd t0, 0(t1)

